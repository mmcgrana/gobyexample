// Test various types of includes
#import <Foundation/Foundation.h>
# import <AppKit/AppKit.h>
#import "stdio.h"
#\
  import \
  "stdlib.h"
# /*line1*/ \
import /* line 2 */ \
"stdlib.h" // line 3

// Commented out code with preprocessor
#if 0
#define MY_NUMBER 3
#endif

 #\
  if 1
#define TEST_NUMBER 3
#endif

// Empty preprocessor
#

// Class forward declaration
@class MyClass;

// Empty classes
@interface EmptyClass
@end
@interface EmptyClass2
{
}
@end
@interface EmptyClass3 : EmptyClass2
{
}
@end

// Custom class inheriting from built-in
@interface MyClass : NSObject
{
@public
  NSString *myString;
  __weak NSString *_weakString;
@protected
  NSTextField *_textField;
@private
  NSDate *privateDate;
}

// Various property aatributes
@property(copy, readwrite, nonatomic) NSString *myString;
@property(weak) NSString *weakString;
@property(retain, strong, atomic) IBOutlet NSTextField *textField;

// Class methods
+ (void)classMethod1:(NSString *)arg;
+ (void)classMethod2:(NSString *) arg; // Test space before arg

@end

typedef id B;

#pragma mark MyMarker

// MyClass.m
// Class extension to declare private property
@interface MyClass ()
@property(retain) NSDate *privateDate;
- (void)hiddenMethod;
@end

// Special category
@interface MyClass (Special)
@property(retain) NSDate *specialDate;
@end

@implementation MyClass
@synthesize myString;
@synthesize privateDate;

- (id)a:(B)b {
  /**
   * C-style comment
   */

  // Selector keywords/types
  SEL someMethod = @selector(hiddenMethod);

  // Boolean types
  Boolean b1 = FALSE;
  BOOL b2 = NO;
  bool b3 = true;

  /**
   * Number literals
   */
  // Int Literal
  NSNumber *n1 = @( 1 );
  // Method call
  NSNumber *n2 = @( [b length] );
  // Define variable
  NSNumber *n3 = @( TEST_NUMBER );
  // Arthimetic expression
  NSNumber *n4 = @(1 + 2);
  // From variable
  int myInt = 5;
  NSNumber *n5 = @(myInt);
  // Nest expression
  NSNumber *n6 = @(1 + (2 + 6.0));
  // Bool literal
  NSNumber *n7 = @NO;
  // Bool expression
  NSNumber *n8 = @(YES);
  // Character
  NSNumber *n9 = @'a';
  // int
  NSNumber *n10 = @123;
  // unsigned
  NSNumber *n11 = @1234U;
  // long
  NSNumber *n12 = @1234567890L;
  // float
  NSNumber *n13 = @3.14F;
  // double
  NSNumber *n14 = @3.14F;
  
  // Array literals
  NSArray *arr = @[ @"1", @"2" ];
  arr = @[ @[ @"1", @"2" ], [arr lastObject] ];
  [arr lastObject];
  [@[ @"1", @"2" ] lastObject];
  
  // Dictionary literals
  NSDictionary *d = @{ @"key": @"value" };
  [[d allKeys] lastObject];
  [[@{ @"key": @"value" } allKeys] lastObject];
  d = @{ @"key": @{ @"key": @"value" } };

  [self hiddenMethod];
  [b length];
  [privateDate class];

  NSDictionary *dictionary = [NSDictionary dictionaryWithObjectsAndKeys:
                              @"1", @"one", @"2", @"two", @"3", @"three", nil];
  
  NSString *key;
  for (key in dictionary) {
    NSLog(@"Number: %@, Word: %@", key, [dictionary valueForKey:key]);
  }

  // Blocks
  int (^myBlock)(int arg1, int arg2);
  NSString *(^myName)(NSString *) = ^(NSString *value) {
    return value;
  };

  return nil;
}

- (void)hiddenMethod {
  // Synchronized block
  @synchronized(self) {
    [myString retain];
    [myString release];
  }
}

+ (void)classMethod1:(NSString *)arg {}
+ (void)classMethod2:(NSString *) arg
{
  // Autorelease pool block
  @autoreleasepool {
    NSLog(@"Hello, World!");
  }
}

@end
