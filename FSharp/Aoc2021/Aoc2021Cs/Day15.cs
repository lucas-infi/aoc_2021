using Xunit;

namespace Aoc2021Cs;

public static class Day15
{
    public static void DoDay15()
    {
        Test();
    }

    public static long NaiveWalk(byte[,] maze)
    {
        (int, int) Pick(int x, int y)
        {
            var right = maze[x + 1, y];
            var down = maze[x, y + 1];
            return down < right ? (x, y + 1) : (x + 1, y);
        }

        var x = 0;
        var y = 0;

        long totalRisk = 0;
        while (x < maze.GetLength(0) - 1 && y < maze.GetLength(1) - 1)
        {
            var newCoord = Pick(x, y);
            x = newCoord.Item1;
            y = newCoord.Item2;
            totalRisk += maze[x, y];
        }

        return totalRisk;
    }

    public static void Test()
    {
        var input = @"
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581";

        var maze = BuilMaze(input);
        var a = NaiveWalk(maze);

        Assert.Equal(40, a);
    }

    private static byte[,] BuilMaze(string input)
    {
        var a = input.Split("\n", StringSplitOptions.RemoveEmptyEntries);
        var xSize = a.First().Length;
        var ySize = a.Length;
        var maze = new byte[xSize, ySize];

        var rowCount = 0;
        foreach (var line in a)
        {
            var colCount = 0;
            foreach (var c in line)
            {
                maze[rowCount, colCount] = (byte)(c - 48);
                colCount++;
            }

            rowCount++;
        }

        return maze;
    }

    public struct Node
    {
        public Node(int x, int y, byte v)
        {
            X = x;
            Y = y;
            V = v;
        }

        public int X { get; }
        public int Y { get; }
        public byte V { get; }

        public List<Node> GetNeighbours(byte[,] map)
        {
            var mapSizeX = map.GetLength(0);
            var mapSizeY = map.GetLength(1);
            if (X >= mapSizeX || Y >= mapSizeY || X < 0 || Y < 0)
            {
                throw new ArgumentOutOfRangeException(
                    $"[{X},{Y}] out of range ({map.GetLength(0)}, {map.GetLength(1)}");
            }

            var l = new List<Node>();
            if (X + 1 < mapSizeX - 1)
            {
                l.Add(new Node(X + 1, Y, map[X + 1, Y]));
            }

            if (X - 1 >= 0)
            {
                l.Add(new Node(X - 1, Y, map[X - 1, Y]));
            }

            if (Y + 1 < mapSizeY - 1)
            {
                l.Add(new Node(X, Y + 1, map[X, Y + 1]));
            }

            if (Y - 1 >= 0)
            {
                l.Add(new Node(X, Y - 1, map[X, Y - 1]));
            }

            return l;
        }
    }
}