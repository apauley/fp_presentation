public class DoubleAllMutation {

    public static void doubleAll(int[] numbers) {
        for (int i=0; i < numbers.length; i++) {
            numbers[i] = numbers[i] * 2;
        }
    }

    public static void main(String[] args) {
        int[] numbers1 = {2,4,7};
        int[] numbers2 = numbers1;
        printArray(numbers1);
        printArray(numbers2);
        doubleAll(numbers1);
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
