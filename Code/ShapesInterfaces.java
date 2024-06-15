interface Shape {
    Point getCenter();
    double getArea();
    void move(double dx, double dy);
}

interface EllipseLike extends Shape {
    double getRadiusX();
    double getRadiusY();
    default double getArea() {
        return Math.PI * getRadiusX() * getRadiusY();
    }
}
interface RectangleLike extends Shape {
    double getWidth();
    double getHeight();
    default double getArea() {
        return getWidth() * getHeight();
    }
}

interface PointLike extends RectangleLike, EllipseLike{
    double getX();
    double getY();
    @Override
    default double getArea() {
        return 0;
    }
}

class Point implements PointLike {
    private double x;
    private double y;

    public Point(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public Point getCenter() {
        return this;
    }

    public double getArea() {
        return 0;
    }

    public void move(double dx, double dy) {
        x += dx;
        y += dy;
    }

    public double getWidth() {
        return 0;
    }

    public double getHeight() {
        return 0;
    }

    public double getRadiusX() {
        return 0;
    }

    public double getRadiusY() {
        return 0;
    }
}

// MAIN
class Main {
    public static void main(String[] args) {
        Point p = new Point(1, 2);
        System.out.println(p.getArea());
    }
}
