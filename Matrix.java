public class Matrix
{
    private int nrows;
    private int ncols;
    private int[][] data;

    /* Create a matrix from existing data */
    public Matrix(int[][] d)
    {
        nrows = d.length;
        ncols = d[0].length;

        data = new int[nrows][ncols];

        /* Copy in the data */
        for (int i = 0; i < nrows; i++)
        {
            for (int j = 0; j < ncols; j++)
            {
                data[i][j] = d[i][j];
            }
        }
    }

    /* Create an empty matrix with the specified dimensions */
    public Matrix(int nr, int nc)
    {
        nrows = nr;
        ncols = nc;
    }
}
