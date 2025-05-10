using System;
using System.IO;

class MatrixProduct
{
    const int N = 20;
    const int ADJ = 4;
    static int[,] matrix = new int[N, N];

    static bool ReadMatrix(string filename)
    {
        try
        {
            using (var reader = new StreamReader(filename))
            {
                for (int i = 0; i < N; ++i)
                {
                    var line = reader.ReadLine();
                    if (line == null) return false;
                    var nums = line.Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
                    if (nums.Length != N) return false;
                    for (int j = 0; j < N; ++j)
                        matrix[i, j] = int.Parse(nums[j]);
                }
            }
            return true;
        }
        catch
        {
            return false;
        }
    }

    static int GreatestProduct(int[,] coords)
    {
        int max = 0;
        for (int i = 0; i < N; ++i)
        {
            for (int j = 0; j < N; ++j)
            {
                // right
                if (j + ADJ <= N)
                {
                    int prod = 1;
                    for (int k = 0; k < ADJ; ++k) prod *= matrix[i, j + k];
                    if (prod > max)
                    {
                        max = prod;
                        for (int k = 0; k < ADJ; ++k) { coords[k, 0] = i; coords[k, 1] = j + k; }
                    }
                }
                // down
                if (i + ADJ <= N)
                {
                    int prod = 1;
                    for (int k = 0; k < ADJ; ++k) prod *= matrix[i + k, j];
                    if (prod > max)
                    {
                        max = prod;
                        for (int k = 0; k < ADJ; ++k) { coords[k, 0] = i + k; coords[k, 1] = j; }
                    }
                }
                // diag down-right
                if (i + ADJ <= N && j + ADJ <= N)
                {
                    int prod = 1;
                    for (int k = 0; k < ADJ; ++k) prod *= matrix[i + k, j + k];
                    if (prod > max)
                    {
                        max = prod;
                        for (int k = 0; k < ADJ; ++k) { coords[k, 0] = i + k; coords[k, 1] = j + k; }
                    }
                }
                // diag down-left
                if (i + ADJ <= N && j - ADJ + 1 >= 0)
                {
                    int prod = 1;
                    for (int k = 0; k < ADJ; ++k) prod *= matrix[i + k, j - k];
                    if (prod > max)
                    {
                        max = prod;
                        for (int k = 0; k < ADJ; ++k) { coords[k, 0] = i + k; coords[k, 1] = j - k; }
                    }
                }
            }
        }
        return max;
    }

    static void PrintMatrixWithHighlight(int[,] coords)
    {
        for (int i = 0; i < N; ++i)
        {
            for (int j = 0; j < N; ++j)
            {
                bool highlight = false;
                for (int k = 0; k < ADJ; ++k)
                    if (coords[k, 0] == i && coords[k, 1] == j) highlight = true;
                if (highlight)
                    Console.Write($"\u001b[31m{matrix[i, j]:D2}\u001b[0m ");
                else
                    Console.Write($"{matrix[i, j]:D2} ");
            }
            Console.WriteLine();
        }
    }

    static void Main(string[] args)
    {
        string filename = args.Length > 0 ? args[0] : "../matrix.txt";
        if (!ReadMatrix(filename))
        {
            Console.Error.WriteLine($"Failed to read matrix from {filename}");
            Environment.Exit(1);
        }
        int[,] coords = new int[ADJ, 2];
        int max = GreatestProduct(coords);
        PrintMatrixWithHighlight(coords);
        Console.WriteLine($"Greatest product of four adjacent numbers: {max}");
    }
}
