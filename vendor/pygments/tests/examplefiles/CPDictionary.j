/*
 * CPDictionary.j
 * Foundation
 *
 * Created by Francisco Tolmasky.
 * Copyright 2008, 280 North, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

//@import "CPRange.j"
@import "CPObject.j"
@import "CPEnumerator.j"
@import "CPException.j"

/* @ignore */
@implementation _CPDictionaryValueEnumerator : CPEnumerator
{
    CPEnumerator    _keyEnumerator;
    CPDictionary    _dictionary;
}

- (id)initWithDictionary:(CPDictionary)aDictionary
{
    self = [super init];
    
    if (self)
    {
        _keyEnumerator = [aDictionary keyEnumerator];
        _dictionary = aDictionary;
    }
    
    return self;
}

- (id)nextObject
{
    var key = [_keyEnumerator nextObject];
    
    if (!key)
        return nil;

    return [_dictionary objectForKey:key];
}

@end

/*! 
    @class CPDictionary
    @ingroup foundation
    @brief A mutable key-value pair collection.

    A dictionary is the standard way of passing around key-value pairs in
    the Cappuccino framework. It is similar to the
    <a href="http://java.sun.com/javase/6/docs/api/index.html">Java map interface</a>,
    except all keys are CPStrings and values can be any
    Cappuccino or JavaScript object.

    If you are familiar with dictionaries in Cocoa, you'll notice that
    there is no CPMutableDictionary class. The regular CPDictionary
    has \c -setObject:forKey: and \c -removeObjectForKey: methods.
    In Cappuccino there is no distinction between immutable and mutable classes.
    They are all mutable.
*/
@implementation CPDictionary : CPObject
{
}

/*
    @ignore
*/
+ (id)alloc
{
    return new objj_dictionary();
}

/*!
    Returns a new empty CPDictionary.
*/
+ (id)dictionary
{
    return [[self alloc] init];
}

/*!
    Returns a new dictionary, initialized with the contents of \c aDictionary.
    @param aDictionary the dictionary to copy key-value pairs from
    @return the new CPDictionary
*/
+ (id)dictionaryWithDictionary:(CPDictionary)aDictionary
{
    return [[self alloc] initWithDictionary:aDictionary];
}

/*!
    Creates a new dictionary with single key-value pair.
    @param anObject the object for the paring
    @param aKey the key for the pairing
    @return the new CPDictionary
*/
+ (id)dictionaryWithObject:(id)anObject forKey:(id)aKey
{
    return [[self alloc] initWithObjects:[anObject] forKeys:[aKey]];
}

/*!
    Creates a dictionary with multiple key-value pairs.
    @param objects the objects to place in the dictionary
    @param keys the keys for each of the objects
    @throws CPInvalidArgumentException if the number of objects and keys is different
    @return the new CPDictionary
*/
+ (id)dictionaryWithObjects:(CPArray)objects forKeys:(CPArray)keys
{
    return [[self alloc] initWithObjects:objects forKeys:keys];
}

/*!
    Creates a dictionary with multiple key-value pairs.
    @param JavaScript object
    @return the new CPDictionary
*/
+ (id)dictionaryWithJSObject:(JSObject)object
{
    return [self dictionaryWithJSObject:object recursively:NO];
}

/*!
    Creates a dictionary with multiple key-value pairs, recursively.
    @param JavaScript object
    @return the new CPDictionary
*/
+ (id)dictionaryWithJSObject:(JSObject)object recursively:(BOOL)recursively
{
    var dictionary = [[self alloc] init];
        
    for (var key in object)
    {
        var value = object[key];
    
        if (recursively && value.constructor === Object)
            value = [CPDictionary dictionaryWithJSObject:value recursively:YES];
    
        [dictionary setObject:value forKey:key];
    }
    
    return dictionary;
}

/*!
    Creates and returns a dictionary constructed by a given pairs of keys and values.
    @param firstObject first object value
    @param ... key for the first object and ongoing value-key pairs for more objects.
    @throws CPInvalidArgumentException if the number of objects and keys is different
    @return the new CPDictionary
    
    Assuming that there's no object retaining in Cappuccino, you can create
    dictionaries same way as with alloc and initWithObjectsAndKeys:
    var dict = [CPDictionary dictionaryWithObjectsAndKeys:
    @"value1", @"key1",
    @"value2", @"key2"];
    
    Note, that there's no final nil like in Objective-C/Cocoa.
    
    @see [CPDictionary initWithObjectsAndKeys:]
*/
+ (id)dictionaryWithObjectsAndKeys:(id)firstObject, ...
{
    arguments[0] = [self alloc];
    arguments[1] = @selector(initWithObjectsAndKeys:);
    
    return objj_msgSend.apply(this, arguments);
}

/*!
    Initializes the dictionary with the contents of another dictionary.
    @param aDictionary the dictionary to copy key-value pairs from
    @return the initialized dictionary
*/
- (id)initWithDictionary:(CPDictionary)aDictionary
{
    var key = "",
        dictionary = [[CPDictionary alloc] init];
    
    for (key in aDictionary._buckets)
        [dictionary setObject:[aDictionary objectForKey:key] forKey:key];
        
    return dictionary;
}

/*!
    Initializes the dictionary from the arrays of keys and objects.
    @param objects the objects to put in the dictionary
    @param keyArray the keys for the objects to put in the dictionary
    @throws CPInvalidArgumentException if the number of objects and keys is different
    @return the initialized dictionary
*/
- (id)initWithObjects:(CPArray)objects forKeys:(CPArray)keyArray
{
    self = [super init];

    if ([objects count] != [keyArray count])
        [CPException raise:CPInvalidArgumentException reason:"Counts are different.("+[objects count]+"!="+[keyArray count]+")"];

    if (self)
    {
        var i = [keyArray count];
        
        while (i--)
            [self setObject:objects[i] forKey:keyArray[i]];
    }
    
    return self;
}

/*!
    Creates and returns a dictionary constructed by a given pairs of keys and values.
    @param firstObject first object value
    @param ... key for the first object and ongoing value-key pairs for more objects.
    @throws CPInvalidArgumentException if the number of objects and keys is different
    @return the new CPDictionary
    
    You can create dictionaries this way:
    var dict = [[CPDictionary alloc] initWithObjectsAndKeys:
    @"value1", @"key1",
    @"value2", @"key2"];
    
    Note, that there's no final nil like in Objective-C/Cocoa.
*/
- (id)initWithObjectsAndKeys:(id)firstObject, ...
{
    var argCount = arguments.length;
    
    if (argCount % 2 !== 0)
        [CPException raise:CPInvalidArgumentException reason:"Key-value count is mismatched. (" + argCount + " arguments passed)"];

    self = [super init];
    
    if (self)
    {
        // The arguments array contains self and _cmd, so the first object is at position 2.
        var index = 2;
        
        for(; index < argCount; index += 2)
        {
            var value = arguments[index];

            if (value === nil)
                break;

            [self setObject:value forKey:arguments[index + 1]];
        }
    }

    return self;
}

/*!
    return a copy of the receiver (does not deep copy the objects contained in the dictionary).
*/
- (CPDictionary)copy
{
    return [CPDictionary dictionaryWithDictionary:self];
}

/*!
    Returns the number of entries in the dictionary
*/
- (int)count
{
    return count;
}

/*!
    Returns an array of keys for all the entries in the dictionary.
*/
- (CPArray)allKeys
{
    return _keys;
}

/*!
    Returns an array of values for all the entries in the dictionary.
*/
- (CPArray)allValues
{
    var index = _keys.length,
        values = [];
        
    while (index--)
        values.push(dictionary_getValue(self, [_keys[index]]));

    return values;
}

/*!
    Returns an enumerator that enumerates over all the dictionary's keys.
*/
- (CPEnumerator)keyEnumerator
{
    return [_keys objectEnumerator];
}

/*!
    Returns an enumerator that enumerates over all the dictionary's values.
*/
- (CPEnumerator)objectEnumerator
{
    return [[_CPDictionaryValueEnumerator alloc] initWithDictionary:self];
}

/*!
    Compare the receiver to this dictionary, and return whether or not they are equal. 
*/
- (BOOL)isEqualToDictionary:(CPDictionary)aDictionary
{
    if (count !== [aDictionary count])
        return NO;

    var index = count;
    while (index--)
    {
        var currentKey = _keys[index],
            lhsObject = _buckets[currentKey],
            rhsObject = aDictionary._buckets[currentKey];

        if (lhsObject === rhsObject)
            continue;
            
        if (lhsObject.isa && rhsObject.isa && [lhsObject respondsToSelector:@selector(isEqual:)] && [lhsObject isEqual:rhsObject])
            continue;
        
        return NO;
    }

    return YES;
}

/*
    Instance.isEqualToDictionary(aDictionary)
    {
        if(this.count()!=aDictionary.count()) return NO;
        
        var i= this._keys.count();
        while(i--) if(this.objectForKey(this._keys[i])!=aDictionary.objectForKey(this._keys[i])) return NO;
        
        return YES;
    }
    
    Instance.allKeys()
    {
        return this._keys;
    }
    
    Instance.allKeysForObject(anObject)
    {
        var i= 0,
            keys= CPArray.array(),
            count= this.count();
        
        while((i= this._objects.indexOfObjectInRage(0, count-i))!=CPNotFound) keys.addObject(this._keys[i]);
        
        return keys;
    }
    
    Instance.allValues()
    {
        return this._objects;
    }
    
    Instance.keyEnumerator()
    {
        return this._keys.objectEnumerator();
    }
    
    Instance.keysSortedByValueUsingSelector(aSelector)
    {
        var dictionary= this,
            objectSelector= function(rhs)
            {
                return aSelector.apply(dictionary.objectForKey(this), [dictionary.objectForKey(rhs)]);
            };
        
        return this._keys.sortedArrayUsingSelector(objectSelector);
    }
    
    Instance.objectEnumerator()
    {
        return this._objects.objectEnumerator();
    }
*/
/*!
    Returns the object for the entry with key \c aKey.
    @param aKey the key for the object's entry
    @return the object for the entry
*/
- (id)objectForKey:(CPString)aKey
{
    var object = _buckets[aKey];
    
    return (object === undefined) ? nil : object;
}
/*
    Instance.objectsForKeys(keys, aNotFoundMarker)
    {
        var i= keys.length,
            objects= CPArray.array();
        
        while(i--)
        {
            var object= this.objectForKey(keys[i]);
            objects.addObject(object==nil?aNotFoundMarker:object);
        }
        
        return objects;
    }
    
    Instance.valueForKey(aKey)
    {
        if(aKey.length && aKey[0]=="@") return this.objectForKey(aKey.substr(1));
        
        return base.valueForKey(aKey);
    }
    
    //
    
    Instance.addEntriesFromDictionary(aDictionary)
    {
        var key,
            keyEnumerator= aDictionary.keyEnumerator();

        while(key= keyEnumerator.nextObject()) this.setObjectForKey(aDictionary.objectForKey(key), key);
    }
*/
/*!
    Removes all the entries from the dictionary.
*/
- (void)removeAllObjects
{
    _keys = [];
    count = 0;
    _buckets = {};
}

/*!
    Removes the entry for the specified key.
    @param aKey the key of the entry to be removed
*/
- (void)removeObjectForKey:(id)aKey
{
    dictionary_removeValue(self, aKey);
}

/*!
    Removes each entry in allKeys from the receiver.
    @param allKeys an array of keys that will be removed from the dictionary
*/
- (void)removeObjectsForKeys:(CPArray)allKeys
{
    var index = allKeys.length;

    while (index--)
        dictionary_removeValue(self, allKeys[index]);
}

/*
    Instance.removeObjectForKey(aKey)
    {
        var entry= this._dictionary[aKey];
        
        if(entry)
        {
            var range= CPMakeRange(entry.index, 1);
            
            this._keys.removeObjectsInRange(range);
            this._objects.removeObjectsInRange(range);
        
            delete this._dictionary[aKey];
        }
    }
    
    Instance.setDictionary(aDictionary)
    {
        this._keys= CPArray.arrayWithArray(aDictionary.allKeys());
        this._objects= CPArray.arrayWithArray(aDictionary.allValues());
        
        this._dictionary= { };
        
        var i= this._keys.count();
        while(i--) this._dictionary[this._keys[i]]= { object: this._objects[i], index: i };
    }
*/
/*!
    Adds an entry into the dictionary.
    @param anObject the object for the entry
    @param aKey the entry's key
*/
- (void)setObject:(id)anObject forKey:(id)aKey
{
    dictionary_setValue(self, aKey, anObject);
}
/*
    Instance.setValueForKey(aValue, aKey)
    {
        if(!aValue) this.removeObjectForKey(aKey);
        else this.setObjectForKey(aValue, aKey);
    }
    
    Instance.copy()
    {
        return CPDictionary.alloc().dictionaryWithDictionary(this);
    }
*/

/*!
    Take all the key/value pairs in aDictionary and apply them to this dictionary.
*/
- (void)addEntriesFromDictionary:(CPDictionary)aDictionary
{
    if (!aDictionary)
        return;
        
    var keys = [aDictionary allKeys],
        index = [keys count];
    
    while (index--)
    {
        var key = keys[index];

        [self setObject:[aDictionary objectForKey:key] forKey:key];
    }
}

/*!
    Returns a human readable description of the dictionary.
*/
- (CPString)description
{
    var description = @"CPDictionary {\n";
    
    var i = _keys.length;
    
    while (i--)
    {
        description += _keys[i] + ":";

        var object = _buckets[_keys[i]];

        if (object && object.isa)
            description += [object description];
        else
            description += object;

        description += "\n";
    }

    description += "}";

    return description;
}

@end

@implementation CPDictionary (CPCoding)

/*
    Initializes the dictionary by unarchiving the data from a coder.
    @param aCoder the coder from which the data will be unarchived.
    @return the initialized dictionary
*/
- (id)initWithCoder:(CPCoder)aCoder
{
    return [aCoder _decodeDictionaryOfObjectsForKey:@"CP.objects"];
}

/*!
    Archives the dictionary to a provided coder.
    @param aCoder the coder to which the dictionary data will be archived.
*/
- (void)encodeWithCoder:(CPCoder)aCoder
{
    [aCoder _encodeDictionaryOfObjects:self forKey:@"CP.objects"];
}

@end

/*!
    @class CPMutableDictionary
    @ingroup compatability

    This class is just an empty subclass of CPDictionary.
    CPDictionary already implements mutable methods and
    this class only exists for source compatability.
*/
@implementation CPMutableDictionary : CPDictionary

@end

objj_dictionary.prototype.isa = CPDictionary;
