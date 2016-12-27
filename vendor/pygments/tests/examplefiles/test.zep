namespace Test;

use Test\Foo;

class Bar
{
    protected a;
    private b;
    public c {set, get};

    public function __construct(string str, boolean bool)
    {
        let this->c = str;
        this->setC(bool);
        let this->b = [];
    }

    public function sayHello(string name)
    {
        echo "Hello " . name;
    }

    protected function loops()
    {
        for a in b {
            echo a;
        }
        loop {
            return "boo!";
        }
    }

}