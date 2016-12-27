class Animal {
    constructor(public name) { }
    move(meters) {
        alert(this.name + " moved " + meters + "m.");
    }
}

class Snake extends Animal {
    constructor(name) { super(name); }
    move() {
        alert("Slithering...");
        super.move(5);
    }
}

class Horse extends Animal {
    constructor(name) { super(name); }
    move() {
        alert("Galloping...");
        super.move(45);
    }
}

@View({
    templateUrl: "app/components/LoginForm.html",
    directives: [FORM_DIRECTIVES, NgIf]
})
@Component({
    selector: "login-form"
})
class LoginForm {

}

var sam = new Snake("Sammy the Python")
var tom: Animal = new Horse("Tommy the Palomino")

sam.move()
tom.move(34)
