abstract class Shape{
    public abstract Point getCenter();
    public abstract void move(int dx, int dy);
}

abstract class EllipseLike extends Shape{
    public abstract double getRadiusX();
    public abstract double getRadiusY();
}

abstract class RectangleLike extends Shape{
    public abstract double getWidth();
    public abstract double getHeight();
}

abstract class PointLike extends RectangleLike{
    public abstract Point getPoint();
}


class Point implements PointLike{
    public int x;
    public int y;

    public Point(int x, int y){
        this.x = x;
        this.y = y;
    }

    public Point getCenter(){
        return this;
    }

    public void move(int dx, int dy){
        x += dx;
        y += dy;
    }

    public double getRadiusX(){
        return 0;
    }

    public double getRadiusY(){
        return 0;
    }

    public double getWidth(){
        return 0;
    }

    public double getHeight(){
        return 0;
    }
}

class Ellipse implements EllipseLike{
    public Point center;
    public double radiusX;
    public double radiusY;

    public Ellipse(Point center, double radiusX, double radiusY){
        this.center = center;
        this.radiusX = radiusX;
        this.radiusY = radiusY;
    }

    public Point getCenter(){
        return this.center;
    }

    public void move(int dx, int dy){
        center.move(dx, dy);
    }

    public double getRadiusX(){
        return this.radiusX;
    }

    public double getRadiusY(){
        return this.radiusY;
    }

    public double getWidth(){
        return this.radiusX * 2;
    }

    public double getHeight(){
        return this.radiusY * 2;
    }
}

class Rectangle implements RectangleLike{
    public Point center;
    public double width;
    public double height;

    public Rectangle(Point center, double width, double height){
        this.center = center;
        this.width = width;
        this.height = height;
    }

    public Point getCenter(){
        return this.center;
    }

    public void move(int dx, int dy){
        center.move(dx, dy);
    }

    public double getWidth(){
        return this.width;
    }

    public double getHeight(){
        return this.height;
    }
}


// main method
class Main {
    public static void main(String[] args) {
        Point p = new Point(1, 2);
        System.out.println(p.getCenter());
        p.move(3, 4);
        System.out.println(p.getCenter());

        Ellipse e = new Ellipse(p, 5, 6);
        System.out.println(e.getCenter());
        e.move(7, 8);
        System.out.println(e.getCenter());

        Rectangle r = new Rectangle(p, 9, 10);
        System.out.println(r.getCenter());
        r.move(11, 12);
        System.out.println(r.getCenter());
    }
}