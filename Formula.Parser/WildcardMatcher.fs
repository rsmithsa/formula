//-----------------------------------------------------------------------
// <copyright file="WildcardMatcher.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser

/// Zero-allocation wildcard matching for the two classic wildcards:
///   '*'  matches zero or more characters (any characters, including '/')
///   '?'  matches exactly one character
/// The whole input must match the whole pattern (anchored, like ^...$ in regex).
///
/// Conceptually equivalent to:
///   Regex.IsMatch(input, "^" + Regex.Escape(pattern).Replace(@"\*", ".*").Replace(@"\?", ".") + "$")
/// but with no regex construction, no allocations, and no backtracking blow-up.
///
/// Complexity: O(n * m) worst case (n = input length, m = pattern length) for
/// patterns with many '*', O(n + m) for patterns with at most one '*'.
/// There is no exponential behaviour because the algorithm only ever
/// backtracks to the most recent '*', never to earlier ones.
module WildcardMatcher =

    open System

    // ---------------------------------------------------------------------
    // Core algorithm
    //
    // This is marked `inline` and takes the character comparison as a
    // function parameter. Because the function is inlined at each call site,
    // the F# compiler substitutes the concrete lambda directly into the loop:
    // no closure allocation, no delegate invocation, and no per-character
    // branch on "ignore case?".
    //
    // Spans are byref-like, so the parameters must be used strictly within
    // this function (no capturing in closures, no storing in fields), which
    // is why the loop uses plain mutable locals rather than recursion.
    // ---------------------------------------------------------------------
    let inline private matchCore
            ([<InlineIfLambda>] equals: char -> char -> bool)
            (s: ReadOnlySpan<char>)
            (p: ReadOnlySpan<char>) : bool =

        let mutable si = 0          // current position in the input string
        let mutable pi = 0          // current position in the pattern

        // Backtrack point. When we pass a '*' we remember where it was (starPi)
        // and how much input it has consumed so far (starSi). If matching later
        // fails, we come back here and let the '*' swallow one more character.
        let mutable starPi = -1     // index of the most recent '*', or -1 if none seen
        let mutable starSi = 0      // input index just after the chars that '*' currently matches

        let mutable failed = false

        while not failed && si < s.Length do
            if pi < p.Length && p.[pi] = '*' then
                // Record this '*' as the new backtrack point.
                // Initially let it match zero characters; we extend it on failure.
                starPi <- pi
                starSi <- si
                pi <- pi + 1

            elif pi < p.Length && (p.[pi] = '?' || equals p.[pi] s.[si]) then
                // Direct hit: '?' matches anything, or the characters are equal.
                si <- si + 1
                pi <- pi + 1

            elif starPi >= 0 then
                // Mismatch, but we have a '*' to fall back on.
                // Rewind the pattern to just after that '*', and make the '*'
                // consume one more input character than it did last time.
                pi <- starPi + 1
                starSi <- starSi + 1
                si <- starSi

            else
                // Mismatch with nothing to backtrack to: definite failure.
                failed <- true

        if failed then
            false
        else
            // Input is exhausted. Any remaining pattern characters must all be '*'
            // (each of which can match the empty string); anything else means the
            // pattern wanted more input than we had.
            while pi < p.Length && p.[pi] = '*' do
                pi <- pi + 1
            pi = p.Length

    // ---------------------------------------------------------------------
    // Comparers
    // ---------------------------------------------------------------------

    /// Exact code-unit equality.
    let inline private ordinalEquals (a: char) (b: char) = a = b

    /// Equality after invariant upper-casing. Upper-casing (rather than lower)
    /// is the conventional choice because a few characters only round-trip
    /// correctly in that direction. The ASCII fast path avoids the
    /// ToUpperInvariant call entirely: ASCII letters differ only in bit 5 (0x20).
    ///
    /// This is "simple" case folding (one char at a time); it does not handle
    /// multi-char foldings such as German 'ß' to "SS".
    let inline private ignoreCaseEquals (a: char) (b: char) =
        if a = b then
            true
        elif (int a ||| int b) < 0x80 then
            let fa = int a ||| 0x20
            let fb = int b ||| 0x20
            fa = fb && uint32 (fa - int 'a') <= uint32 (int 'z' - int 'a')
        else
            Char.ToUpperInvariant a = Char.ToUpperInvariant b

    // ---------------------------------------------------------------------
    // Public API
    // ---------------------------------------------------------------------

    /// Case-sensitive match using ordinal (exact code-unit) comparison.
    let isMatch (input: ReadOnlySpan<char>) (pattern: ReadOnlySpan<char>) : bool =
        matchCore ordinalEquals input pattern

    /// Case-insensitive match using invariant upper-case folding.
    let isMatchIgnoreCase (input: ReadOnlySpan<char>) (pattern: ReadOnlySpan<char>) : bool =
        matchCore ignoreCaseEquals input pattern
