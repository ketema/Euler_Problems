using System;

class MatrixProductTest
{
    const int N = 4;
    const int ADJ = 4;
    static int[,] matrix = new int[N, N]
    {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12},
        {13, 14, 15, 16}
    };

    static int GreatestProduct(int[,] matrix, int[,] coords)
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

    static void TestGreatestProduct()
    {
        int[,] coords = new int[ADJ, 2];
        int result = GreatestProduct(matrix, coords);
        if (result != 43680)
            throw new Exception($"Expected 43680, got {result}");
        Console.WriteLine("TestGreatestProduct passed!");
    }

    static void Main()
    {
        TestGreatestProduct();
        Console.WriteLine("All tests passed!");
    }
}
