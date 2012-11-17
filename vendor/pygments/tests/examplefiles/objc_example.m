#import "Somefile.h"

@implementation ABC

- (id)a:(B)b {
    return 1;
}

@end

@implementation ABC

- (void)xyz;

@end

NSDictionary *dictionary = [NSDictionary dictionaryWithObjectsAndKeys:
    @"quattuor", @"four", @"quinque", @"five", @"sex", @"six", nil];


NSString *key;
for (key in dictionary) {
    NSLog(@"English: %@, Latin: %@", key, [dictionary valueForKey:key]);
}

