// Created by Lionello Lunesu and placed in the public domain.
// This file has been modified from its original version.
// It has been formatted to fit your screen.
module phoneno;     // optional
import std.stdio;   // writefln     
import std.ctype;   // isdigit     
import std.stream;  // BufferedFile

// Just for readability (imagine char[][][char[]])    
alias char[] string;
alias string[] stringarray;

/// Strips non-digit characters from the string (COW)
string stripNonDigit( in string line ) 
{
    string ret;
    foreach(uint i, c; line) {
        // Error: std.ctype.isdigit at C:\dmd\src\phobos\std\ctype.d(37) 
        // conflicts with std.stream.isdigit at C:\dmd\src\phobos\std\stream.d(2924)
        if (!std.ctype.isdigit(c)) {
            if (!ret)
                ret = line[0..i];    
        }    
        else if (ret)
            ret ~= c;    
    }    
    return ret?ret:line;
}

unittest {
    assert( stripNonDigit("asdf") == ""  );
    assert( stripNonDigit("\'13-=2 4kop") ==  "1324"  );
}

/// Converts a word into a number, ignoring all non alpha characters  
string wordToNum( in string word )
{
// translation table for the task at hand
const char[256] TRANSLATE =    
    "                                "  // 0   
    "                0123456789      "  // 32     
    " 57630499617851881234762239     "  // 64   
    " 57630499617851881234762239     "
    "                                "
    "                                "
    "                                "    
    "                                ";
    string ret;
    foreach(c; cast(ubyte[])word)
        if (TRANSLATE[c] != ' ')
            ret ~= TRANSLATE[c];
    return ret;
}

unittest {
 // Test wordToNum using the table from the task description.
 assert( "01112223334455666777888999" ==
   wordToNum("E | J N Q | R W X | D S Y | F T | A M | C I V | B K U | L O P | G H Z"));
 assert( "01112223334455666777888999" == 
   wordToNum("e | j n q | r w x | d s y | f t | a m | c i v | b k u | l o p | g h z"));
 assert( "0123456789" == 
   wordToNum("0 |   1   |   2   |   3   |  4  |  5  |   6   |   7   |   8   |   9"));
}

void main( string[] args )
{
    // This associative array maps a number to an array of words.    
    stringarray[string]    num2words;

    foreach(string word; new BufferedFile("dictionary.txt" ) )
        num2words[ wordToNum(word) ] ~= word.dup;        // must dup

    /// Finds all alternatives for the given number
    /// (should have been stripped from non-digit characters)
    stringarray _FindWords( string numbers, bool digitok )
    in {
        assert(numbers.length >  0);    
    }    
    out(result) {
        foreach (a; result)
            assert( wordToNum(a) == numbers );
    }    
    body {
        stringarray ret;
        bool foundword = false;
        for (uint t=1; t<=numbers.length; ++t) {
            auto alternatives = numbers[0..t] in num2words;
            if (!alternatives)
                continue;
            foundword = true;
            if (numbers.length >  t) {
                // Combine all current alternatives with all alternatives     
                // of the rest (next piece can start with a digit)              
                foreach (a2; _FindWords( numbers[t..$], true     ) )
                    foreach(a1; *alternatives)
                       ret ~= a1 ~ " " ~ a2;
            }
            else    
                ret ~= *alternatives;    // append these alternatives
        }
        // Try to keep 1 digit, only if we're allowed and no other
        // alternatives were found
        // Testing "ret.length" makes more sense than testing "foundword",
        // but the other implementations seem to do just this.
        if (digitok && !foundword) { //ret.length == 0  
            if(numbers.length >  1) {
                // Combine 1 digit with all altenatives from the rest    
                // (next piece can not start with a digit)          
                foreach (a; _FindWords( numbers[1..$], false ) )
                    ret ~= numbers[0..1] ~ " " ~ a;
            }    
            else    
                ret ~= numbers[0..1];    // just append this digit             
        }    
        return ret;
    }

    /// (This function was inlined in the original program) 
    /// Finds all alternatives for the given phone number 
    /// Returns: array of strings 
    stringarray FindWords( string phone_number )
    {
        if (!phone_number.length)
            return null;
        // Strip the non-digit characters from the phone number, and
        // pass it to the recursive function (leading digit is allowed)
        return _FindWords( stripNonDigit(phone_number), true );    
    }    
    
    // Read the phone numbers     
    foreach(string phone; new BufferedFile("input.txt"   ) )
        foreach(alternative; FindWords( phone ) )
            writefln(phone, ": ", alternative );
}

