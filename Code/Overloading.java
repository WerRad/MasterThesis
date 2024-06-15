class Rectangle{
    private double width;
    private double height;

    public Rectangle(double width, double height){
        this.width = width;
        this.height = height;
    }

    public double getWidth(){
        return this.width;
    }

    public double getHeight(){
        return this.height;
    }

}

class Square{
    private double side;

    public Square(double side){
        this.side = side;
    }

    public double getSide(){
        return this.side;
    }

}

// Class for getting area
/**
 * Overloading
 */
public class Overloading {

    public static double getArea(Rectangle r){
        return r.getWidth() * r.getHeight();
    }

    public static double getArea(Square s){
        return s.getSide() * s.getSide();
    }

    public static void main(String[] args) {
        Rectangle r = new Rectangle(5, 10);
        Square s = new Square(5);

        System.out.println("Area of rectangle: " + getArea(r));
        System.out.println("Area of square: " + getArea(s));
    }

}