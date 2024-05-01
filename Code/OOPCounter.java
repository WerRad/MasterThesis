class OOPCounter {
    private char c;
    private String counterDescription;
    private int count;

    // constructor
    public OOPCounter(char c, String counterDescription) {
        this.c = c;
        this.counterDescription = counterDescription;
        this.count = 0;
    }

    // method to count the number of occurrences of character c in the string
    public int countOccurrences(String str) {
        for (int i = 0; i < str.length(); i++) {
            if (str.charAt(i) == c) {
                count++;
            }
        }
        return count;
    }

    // method to get the counter description
    public String getCounterDescription() {
        return counterDescription;
    }
}

// main method
class Main {
    public static void main(String[] args) {
        OOPCounter counter = new OOPCounter('a', "Count the number of 'a' in the string");
        String str = "Java is a programming language";
        System.out.println(counter.getCounterDescription());
        System.out.println("Number of 'a' in the string: " + counter.countOccurrences(str));
    }
}