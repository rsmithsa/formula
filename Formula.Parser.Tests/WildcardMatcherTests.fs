//-----------------------------------------------------------------------
// <copyright file="WildcardMatcherTests.fs" company="Richard Smith">
//     Copyright (c) Richard Smith. All rights reserved.
// </copyright>
//-----------------------------------------------------------------------

namespace Formula.Parser.Tests

open System
open System.Diagnostics
open System.Text.RegularExpressions
open Microsoft.VisualStudio.TestTools.UnitTesting

open Formula.Parser

[<TestClass>]
type WildcardMatcherTests () =

    // Spans are byref-like and can't be captured in closures, so wrap the
    // public API in string-based helpers for use in the tests below.
    static let isMatch (input: string) (pattern: string) =
        WildcardMatcher.isMatch (input.AsSpan()) (pattern.AsSpan())

    static let isMatchIgnoreCase (input: string) (pattern: string) =
        WildcardMatcher.isMatchIgnoreCase (input.AsSpan()) (pattern.AsSpan())

    // Reference implementation. Singleline makes '.' match '\n', and \z anchors
    // at the true end of input ($ would also match before a trailing '\n').
    static let regexMatch (options: RegexOptions) (input: string) (pattern: string) =
        let regex = "^" + Regex.Escape(pattern).Replace(@"\*", ".*").Replace(@"\?", ".") + @"\z"
        Regex.IsMatch(input, regex, options ||| RegexOptions.Singleline)

    static let assertMatch (matcher: string -> string -> bool) input pattern =
        Assert.IsTrue(matcher input pattern, sprintf "Expected '%s' to match pattern '%s'" input pattern)

    static let assertNoMatch (matcher: string -> string -> bool) input pattern =
        Assert.IsFalse(matcher input pattern, sprintf "Expected '%s' not to match pattern '%s'" input pattern)

    [<TestMethod>]
    member this.TestEmpty () =
        assertMatch isMatch "" ""
        assertMatch isMatch "" "*"
        assertMatch isMatch "" "***"
        assertNoMatch isMatch "" "?"
        assertNoMatch isMatch "" "a"
        assertNoMatch isMatch "" "*?"
        assertNoMatch isMatch "a" ""

    [<TestMethod>]
    member this.TestLiteral () =
        assertMatch isMatch "abc" "abc"
        assertNoMatch isMatch "abc" "abd"
        assertNoMatch isMatch "abc" "ab"
        assertNoMatch isMatch "ab" "abc"
        assertNoMatch isMatch "abc" "bc"
        assertNoMatch isMatch "abc" "Abc"

    [<TestMethod>]
    member this.TestQuestionMark () =
        assertMatch isMatch "a" "?"
        assertMatch isMatch "abc" "a?c"
        assertMatch isMatch "abc" "???"
        assertNoMatch isMatch "abc" "??"
        assertNoMatch isMatch "abc" "????"
        assertNoMatch isMatch "ac" "a?c"
        assertMatch isMatch "a\nc" "a?c"

    [<TestMethod>]
    member this.TestStar () =
        assertMatch isMatch "abc" "*"
        assertMatch isMatch "abc" "a*"
        assertMatch isMatch "abc" "*c"
        assertMatch isMatch "abc" "a*c"
        assertMatch isMatch "ac" "a*c"
        assertMatch isMatch "abc" "*b*"
        assertMatch isMatch "abc" "abc*"
        assertMatch isMatch "abc" "*abc"
        assertMatch isMatch "abc" "a**c"
        assertNoMatch isMatch "abc" "a*d"
        assertNoMatch isMatch "abc" "*d*"
        assertNoMatch isMatch "abc" "b*"
        assertNoMatch isMatch "abc" "*b"

    [<TestMethod>]
    member this.TestStarMatchesAnyCharacter () =
        assertMatch isMatch "a/b/c" "a*c"
        assertMatch isMatch "a\nb\nc" "a*c"
        assertMatch isMatch "a.b" "a*b"
        assertMatch isMatch "line\n" "*"

    [<TestMethod>]
    member this.TestCombined () =
        assertMatch isMatch "abcde" "a?c*"
        assertMatch isMatch "abcde" "*c?e"
        assertMatch isMatch "abcde" "?*?"
        assertNoMatch isMatch "a" "?*?"
        assertMatch isMatch "ab" "*?"
        assertMatch isMatch "ab" "?*"
        assertNoMatch isMatch "abcde" "*c??e"

    [<TestMethod>]
    member this.TestBacktracking () =
        // The first candidate for the '*' fails; the matcher must retry later positions.
        assertMatch isMatch "aaab" "*ab"
        assertMatch isMatch "abcabd" "*abd"
        assertMatch isMatch "mississippi" "m*iss*ppi"
        assertMatch isMatch "mississippi" "*sip*"
        assertNoMatch isMatch "mississippi" "m*iss*ppx"
        assertMatch isMatch "abxbxbyc" "a*b?c"
        assertNoMatch isMatch "abxbxbc" "a*b?c"
        assertMatch isMatch "Revenue.2024.Q1" "Revenue.*.Q?"
        assertNoMatch isMatch "Revenue.2024.Q10" "Revenue.*.Q?"

    [<TestMethod>]
    member this.TestWildcardCharactersInInput () =
        // There is no escaping: '*' and '?' in the pattern are always wildcards,
        // and in the input they are ordinary characters.
        assertMatch isMatch "*" "*"
        assertMatch isMatch "?" "?"
        assertMatch isMatch "a*b" "a?b"
        assertNoMatch isMatch "*" "a"
        assertNoMatch isMatch "?" "a"

    [<TestMethod>]
    member this.TestRegexMetacharactersAreLiteral () =
        assertMatch isMatch "a.c" "a.c"
        assertNoMatch isMatch "abc" "a.c"
        assertMatch isMatch "[x]+(y)^$|\\" "[x]+(y)^$|\\"
        assertNoMatch isMatch "x" "[x]"

    [<TestMethod>]
    member this.TestCaseSensitive () =
        assertNoMatch isMatch "ABC" "abc"
        assertNoMatch isMatch "abc" "a*C"
        assertNoMatch isMatch "é" "É"

    [<TestMethod>]
    member this.TestIgnoreCaseAscii () =
        assertMatch isMatchIgnoreCase "ABC" "abc"
        assertMatch isMatchIgnoreCase "abc" "ABC"
        assertMatch isMatchIgnoreCase "aBc" "AbC"
        assertMatch isMatchIgnoreCase "Hello World" "h*O w?RLD"
        assertMatch isMatchIgnoreCase "123-_." "123-_."
        assertNoMatch isMatchIgnoreCase "abc" "abd"
        assertNoMatch isMatchIgnoreCase "abc" "ab"

    [<TestMethod>]
    member this.TestIgnoreCaseAsciiNonLetters () =
        // These pairs differ only in bit 5 (0x20) but are not letters, so must not match.
        assertNoMatch isMatchIgnoreCase "@" "`"
        assertNoMatch isMatchIgnoreCase "[" "{"
        assertNoMatch isMatchIgnoreCase "]" "}"
        assertNoMatch isMatchIgnoreCase "^" "~"
        assertNoMatch isMatchIgnoreCase "\\" "|"
        assertNoMatch isMatchIgnoreCase "_" "\u007F"
        assertNoMatch isMatchIgnoreCase "0" "\u0010"
        assertNoMatch isMatchIgnoreCase " " "\u0000"
        assertMatch isMatchIgnoreCase "@" "@"

    [<TestMethod>]
    member this.TestIgnoreCaseAsciiBoundaries () =
        assertMatch isMatchIgnoreCase "A" "a"
        assertMatch isMatchIgnoreCase "Z" "z"
        assertNoMatch isMatchIgnoreCase "A" "z"
        assertNoMatch isMatchIgnoreCase "Z" "a"

    [<TestMethod>]
    member this.TestIgnoreCaseNonAscii () =
        assertMatch isMatchIgnoreCase "é" "É"
        assertMatch isMatchIgnoreCase "ÀÉÎÕÜ" "àéîõü"
        assertMatch isMatchIgnoreCase "ΑΒΓ" "αβγ"
        assertMatch isMatchIgnoreCase "ПРИВЕТ" "привет"
        assertMatch isMatchIgnoreCase "Straße" "STRA?E"
        assertNoMatch isMatchIgnoreCase "é" "e"
        // Simple case folding only: 'ß' does not fold to "SS".
        assertNoMatch isMatchIgnoreCase "Straße" "STRASSE"

    [<TestMethod>]
    member this.TestIgnoreCaseMixedAsciiAndNonAscii () =
        // One char ASCII, the other not: must take the non-ASCII path and not match.
        assertNoMatch isMatchIgnoreCase "a" "à"
        assertNoMatch isMatchIgnoreCase "A" "À"

    [<TestMethod>]
    member this.TestIgnoreCaseWithWildcards () =
        assertMatch isMatchIgnoreCase "MISSISSIPPI" "m*iss*ppi"
        assertMatch isMatchIgnoreCase "revenue.2024.q1" "REVENUE.*.Q?"
        assertNoMatch isMatchIgnoreCase "revenue.2024.q10" "REVENUE.*.Q?"

    [<TestMethod>]
    member this.TestSubstringSpans () =
        let input = "xxabcxx".AsSpan().Slice(2, 3)
        let pattern = "[a*c]".AsSpan().Slice(1, 3)
        Assert.IsTrue(WildcardMatcher.isMatch input pattern)
        Assert.IsTrue(WildcardMatcher.isMatchIgnoreCase input pattern)

    [<TestMethod>]
    member this.TestPathologicalPatternDoesNotBlowUp () =
        // Exponential for naive recursive backtracking; must be fast here.
        let input = String('a', 5000)
        let pattern = String.replicate 50 "*a" + "*b"
        let sw = Stopwatch.StartNew()
        assertNoMatch isMatch input pattern
        assertMatch isMatch (input + "b") pattern
        sw.Stop()
        Assert.IsTrue(sw.ElapsedMilliseconds < 2000L, sprintf "Took %dms" sw.ElapsedMilliseconds)

    [<TestMethod>]
    member this.TestMatchesRegexReference () =
        let rnd = Random(12345)
        let inputAlphabet = "abAB*?\n"
        let patternAlphabet = "abAB**??\n"
        let randomString (alphabet: string) maxLen =
            String(Array.init (rnd.Next(maxLen + 1)) (fun _ -> alphabet.[rnd.Next(alphabet.Length)]))

        for _ in 1 .. 20000 do
            let input = randomString inputAlphabet 10
            let pattern = randomString patternAlphabet 8
            let expected = regexMatch RegexOptions.None input pattern
            Assert.AreEqual(expected, isMatch input pattern, sprintf "Input: %A Pattern: %A" input pattern)

    [<TestMethod>]
    member this.TestIgnoreCaseMatchesRegexReference () =
        // ASCII only: regex case-insensitivity uses its own equivalence tables,
        // which differ from ToUpperInvariant for some non-ASCII characters.
        let rnd = Random(54321)
        let inputAlphabet = "aAzZ@`[{"
        let patternAlphabet = "aAzZ@`[{*?"
        let randomString (alphabet: string) maxLen =
            String(Array.init (rnd.Next(maxLen + 1)) (fun _ -> alphabet.[rnd.Next(alphabet.Length)]))

        for _ in 1 .. 20000 do
            let input = randomString inputAlphabet 10
            let pattern = randomString patternAlphabet 8
            let expected = regexMatch (RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant) input pattern
            Assert.AreEqual(expected, isMatchIgnoreCase input pattern, sprintf "Input: %A Pattern: %A" input pattern)
