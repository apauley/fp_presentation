public class DoubleAllIterative {

    public static int[] doubleAll(int[] numbers) {
        int[] nums2 = new int[numbers.length];
        for (int i=0; i < numbers.length; i++) {
            nums2[i] = numbers[i] * 2;
        }
        return nums2;
    }

    public static void main(String[] args) {
        int[] numbers1 = {2,4,7};
        int[] numbers2 = numbers1;
        printArray(numbers1);
        printArray(numbers2);
        int[] numbers3 = doubleAll(numbers1);
        printArray(numbers3);
        printArray(numbers1);
        printArray(numbers2);
    }

    public static void printArray(int[] numbers) {
        for (int i=0; i < numbers.length ; i++) {
            System.out.print(" " + numbers[i] + " ");
        }
        System.out.println();
    }
}
