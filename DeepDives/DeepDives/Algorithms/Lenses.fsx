type Car = { 
    Make: string 
    Model: string 
    Mileage: int 
}

type Editor = { 
    Name: string 
    Salary: int 
    Car: Car 
}

type Book = { 
    Name: string 
    Author: string 
    Editor: Editor 
}


type Lens<'a,'b> = {
    Get: 'a -> 'b
    Set: 'b -> 'a -> 'a
}
with member l.Update f a = 
            let value = l.Get a 
            let newValue = f value // f will be e.g. "Set 1", which will be applied to the current property value (which is a lens as well)
            l.Set newValue a


let inline (>>|) (l1: Lens<_,_>) (l2: Lens<_,_>) = 
    { Get = l1.Get >> l2.Get 
      Set = l2.Set >> l1.Update } // will wait for the new value of l2 and then replace the l1 with modified l2

let inline (+=) (l: Lens<_,_>) v = l.Update ((+) v)

let inline (:=) (l: Lens<_,_>) v = l.Set v

type Car with
    static member mileage = 
        { Get = fun (c: Car) -> c.Mileage
          Set = fun v (x: Car) -> { x with Mileage = v } }

type Editor with
    static member car = 
        { Get = fun (x: Editor) -> x.Car 
          Set = fun v (x: Editor) -> { x with Car = v } }

type Book with
    static member editor = 
        { Get = fun (x: Book) -> x.Editor 
          Set = fun v (x: Book) -> { x with Editor = v } }


let bookEditorCarMileage = Book.editor >>| Editor.car >>| Car.mileage


let aBook = {Name = "Autobiography"; Author = "Anonymous";
             Editor = {Name = "John"; Salary = 100; 
                      Car = {Make = "Ford"; Model = "Mustang"; Mileage = 10000}}}

let mileage = bookEditorCarMileage.Get aBook

let b1 = aBook |> bookEditorCarMileage.Set (mileage+1000)

let b2 = aBook |> bookEditorCarMileage += 1000

let b3 = aBook |> (bookEditorCarMileage := 11000)




