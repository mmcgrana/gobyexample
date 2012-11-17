// MyClass.h
@interface MyClass : NSObject
{
    NSString *value;
    NSTextField *textField;
@private
    NSDate *lastModifiedDate;
}
@property(copy, readwrite) NSString *value;
@property(retain) IBOutlet NSTextField *textField;
@end

// MyClass.m
// Class extension to declare private property
@interface MyClass ()
@property(retain) NSDate *lastModifiedDate;
@end

@implementation MyClass
@synthesize value;
@synthesize textField;
@synthesize lastModifiedDate;
// implementation continues
@end
