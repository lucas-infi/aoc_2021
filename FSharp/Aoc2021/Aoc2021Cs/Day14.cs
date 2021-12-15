using System.Collections.Immutable;
using Xunit;

namespace Aoc2021Cs;

public static class Day14
{
    public static void DoDay14()
    {
        Test();

        var input = File.ReadAllText("input/day14.txt");
        var r = DoTenSteps(input);
        var rr = DoFortySteps(input);
        Console.WriteLine($"Day14 {r} {rr}");
    }

    public static IEnumerable<char> PolymerStep(char[] current, Dictionary<DiAtom, char> insertionInstructions)
    {
        var buff = new List<char>();
        var last = current.Last();

        for (int i = 0; i < current.Length - 1; i++)
        {
            var s = new DiAtom(current[i], current[i + 1]);
            var a = insertionInstructions[s];
            var triplet = new[] { current[i], a };
            buff.AddRange(triplet);
        }

        buff.Add(last);
        return buff;
    }

    public static Dictionary<DiAtom, long> PolyCounter(
        Dictionary<DiAtom, char> insertionInstructions,
        Dictionary<DiAtom, long> cache)
    {
        var newPairs = new Dictionary<DiAtom, long>();
        foreach (var kv in cache)
        {
            var atom = kv.Key;
            var amount = kv.Value;
            var newB = insertionInstructions[atom];
            var newAtomL = new DiAtom(atom.A, newB);
            var newAtomR = new DiAtom(newB, atom.B);

            AddOrCountPair(newAtomL, newPairs, amount);
            AddOrCountPair(newAtomR, newPairs, amount);
        }

        // Should work fine, since we already know these are not in the dictionary
        return newPairs;
    }

    private static void AddOrCountPair(
        DiAtom newAtom,
        Dictionary<DiAtom, long> newPairs,
        long amountToAdd)
    {
        if (newPairs.ContainsKey(newAtom))
            newPairs[newAtom] += amountToAdd;
        else
            newPairs.Add(newAtom, amountToAdd);
    }

    public static Dictionary<DiAtom, char> BuildInsertions(IEnumerable<string> input) =>
        input
            .Select(s => s.Split(" -> ", StringSplitOptions.RemoveEmptyEntries))
            .ToDictionary(a => new DiAtom(a[0][0], a[0][1]), a => a[1][0]);

    public static long DoTenSteps(string inputStr)
    {
        var input = inputStr.Split("\n", StringSplitOptions.RemoveEmptyEntries);
        var poly = input.First().ToArray();
        var instructions = BuildInsertions(input.Skip(1));

        for (int i = 0; i < 10; i++)
        {
            poly = PolymerStep(poly, instructions).ToArray();
        }

        var grouped = poly
            .GroupBy(v => v, c => 1L, (c, ints) => (c, ints.Sum()))
            .OrderByDescending(tuple => tuple.Item2)
            .ToList();

        return grouped.First().Item2 - grouped.Last().Item2;
    }

    public static long DoFortySteps(string inputStr)
    {
        var input = inputStr.Split("\n", StringSplitOptions.RemoveEmptyEntries);
        var poly = input.First().ToArray();
        var instructions = BuildInsertions(input.Skip(1));

        var cache = new Dictionary<DiAtom, long>();


        for (int i = 0; i < poly.Length - 1; i++)
        {
            var a = new DiAtom(poly[i], poly[i + 1]);
            if (cache.ContainsKey(a))
                cache[a] += 1;
            else
                cache[a] = 1;
        }

        for (int i = 0; i < 40; i++)
        {
            cache = PolyCounter(instructions, cache);
        }

        var grouped =
            cache.Select(kv => new[] { (kv.Key.A, kv.Value), (kv.Key.B, kv.Value) })
                .SelectMany(kv => kv)
                .GroupBy(v => v.Item1, c => c.Value, (c, ints) => (c, Math.Ceiling(ints.Sum() / 2.0)))
                .OrderByDescending(tuple => tuple.Item2)
                .ToList();

        return Convert.ToInt64(grouped.First().Item2 - grouped.Last().Item2);
    }

    public static void Test()
    {
        var inputStr = @"NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C";
        var r = DoTenSteps(inputStr);
        var rr = DoFortySteps(inputStr);
        Assert.Equal(1588, r);
        Assert.Equal(2188189693529L, rr);
    }

    public struct DiAtom
    {
        public readonly char A;
        public readonly char B;

        public DiAtom(char a, char b)
        {
            A = a;
            B = b;
        }

        public override string ToString()
        {
            return $"DiAtom: [{A}{B}]";
        }
    }
}