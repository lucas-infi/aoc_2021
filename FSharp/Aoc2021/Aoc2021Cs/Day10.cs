using Xunit;

namespace Aoc2021Cs;

public class Day10
{
    public static void DoDay10()
    {
        RunTest();
        var inputStr =
            File.ReadAllText("input/day10.txt")
                .Split("\n", StringSplitOptions.RemoveEmptyEntries);

        var score =
            inputStr
                .Select(FindMissingBraceOrZero)
                .ToList();
        var summed = score.Select(GetScore).Sum();

        Console.WriteLine($"Day10 {summed} {0}");
    }

    private static char FindMissingBraceOrZero(string line)
    {
        var stack = new Stack<char>();
        foreach (var c in line)
        {
            if (IsOpening(c))
            {
                stack.Push(c);
                continue;
            }

            if (IsClosingFor(stack.Peek(), c))
            {
                stack.Pop();
                continue;
            }

            return c;
        }

        return '0';
    }

    private static int GetScore(char c) =>
        c switch
        {
            ')' => 3,
            '}' => 57,
            ']' => 1197,
            '>' => 25137,
            _ => 0,
        };

    private static bool IsOpening(char c) =>
        c switch
        {
            '(' => true,
            '{' => true,
            '[' => true,
            '<' => true,
            _ => false,
        };

    private static bool IsClosing(char c) =>
        c switch
        {
            ')' => true,
            '}' => true,
            ']' => true,
            '>' => true,
            _ => false,
        };

    private static bool IsClosingFor(char open, char c) =>
        open switch
        {
            '(' => c == ')',
            '{' => c == '}',
            '[' => c == ']',
            '<' => c == '>',
            _ => false,
        };

    private static void RunTest()
    {
        var input = @"[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]".Split("\n", StringSplitOptions.RemoveEmptyEntries);
        Assert.Equal('0', FindMissingBraceOrZero("()"));
        Assert.Equal('0', FindMissingBraceOrZero("{}"));
        Assert.Equal('0', FindMissingBraceOrZero("[]"));
        Assert.Equal('0', FindMissingBraceOrZero("([])"));
        Assert.Equal('0', FindMissingBraceOrZero("([][][])"));
        Assert.Equal('0', FindMissingBraceOrZero("(())"));
        Assert.Equal('0', FindMissingBraceOrZero("[<>({}){}[([])<>]]"));
        Assert.Equal('0', FindMissingBraceOrZero("(((((((((())))))))))"));
        Assert.Equal('0', FindMissingBraceOrZero("((())"));
        Assert.Equal('}', FindMissingBraceOrZero("(}"));
        Assert.Equal('}', FindMissingBraceOrZero("[(}]"));
        Assert.Equal('>', FindMissingBraceOrZero("{()()()>"));
        Assert.Equal('}', FindMissingBraceOrZero("(((()))}"));
        Assert.Equal(')', FindMissingBraceOrZero("<([]){()}[{}])"));
        Assert.Equal(')', FindMissingBraceOrZero("((<([]){()}[{}]))){{}}"));
        Assert.Equal('}', FindMissingBraceOrZero("(())(()}(())(}[[]]"));


        var score = input.Select(FindMissingBraceOrZero).ToList();
        Assert.Equal(26397, score.Select(GetScore).Sum());
    }
}
