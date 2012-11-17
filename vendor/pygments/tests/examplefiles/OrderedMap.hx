package util;

import util.Map;
import util.Collection;
import util.Set;
import util.Option;
import util.Debug;
import util.Throwable;

using util.StringFormat;

/**
 * An ordered map of (key,value) pairs. The key ordering is defined by
 * a comparison function specified at construction. Duplicate keys
 * are not allowed.
 *
 * Worst Case Time and Space Complexities:
 * [operation]   [time]      [space]
 * insert        O(lg(n))    O(lg(n))
 * find          O(lg(n))    O(1)
 * delete        O(lg(n))    O(lg(n))
 * range-query   O(lg(n))*   O(lg(n))
 * iteration     O(n)**      O(lg(n))
 *   *range-query returns an iterator over elements in the range
 *   **total cost of iterating over the entire map
 *
 * The map is backed by a Left-Leaning Red-Black 2-3 Tree
 * adapted from Robert Sedgewick (2008) (http://www.cs.princeton.edu/~rs/)
 *
 * Implementation choices (let size of tree be n)
 * - Parent Pointers
 *   - This implementation omits parent pointers.
 *   - Omitting parent pointers saves n words of persistent memory
 *     at the expense of lg(n) stack space per operation.
 *   - Without parent pointers, most operations in the tree must
 *     either use recursion, or simulate recursion by saving a history
 *     of nodes via a stack. For example, each iterator will require
 *     lg(n) extra space to track progress through the tree. Insertions
 *     and deletions into the tree will also invalidate any existing
 *     iterators.
 * - Node Size Information
 *   - This implementation omits the size of each node.
 *   - Omitting size information saves n words of long-term memory at
 *     the expense of not providing a find-kth operation.
 *   - This seems like a reasonable trade-off as range queries are
 *     generally more common than find-kth operations. The implementation
 *     used below could easily be modified to provide a version with
 *     size information should find-kth be of specific interest.
 * - Recursive vs. Iterative
 *   - This implementation uses recursive algorithms.
 *   - The recursive implementations allow the code to remain compact and
 *     understandable. Since the height of LLRB 2-3 Trees is gaurenteed
 *     to be at most 2lg(n), stack overflow is typically not a concern.
 *     Unlike the standard single-rotation red-black algorithm, LLRB
 *     operations are not tail-recursive, so even an iterative
 *     version will require lg(n) extra memory.
 */
class OrderedMap<K,V>
{
  private var root  :Null<Node<K,V>>;
  private var nodeCount :Int;
  private var comp  :K -> K -> Int;

  public function new( keyComp :K -> K -> Int )
  {
    root = null;
    comp = keyComp;
    nodeCount = 0;
    assertInvariants();
  }

  /**
   * @returns Some(v) if (\key,v) is in the map, None otherwise.
   */
  public function get(key :K) :Option<V>
  {
    //normal BST search
    var n = root;
    while( n != null )
    {
      var cmp = comp(key,n.key);
      if( cmp < 0 )
      {
        n = n.left;
      }
      else if ( cmp > 0 )
      {
        n = n.right;
      }
      else
      {
        return Some(n.val);
      }
    }
    return None;
  }

  /**
   * Puts (\key,\val) into the map or replaces the current value of \key
   * with \val.
   *
   * @return None if \key currently is not in the map, or Some(v) if (\key,v)
   *   was in the map before the put operation.
   */
  public function set(key :K, val :V) :Option<V>
  {
    var ret = new Ref<V>(null);
    root = insertNode(root,key,val,ret);
    root.color = black;

    assertInvariants();

    if( ret.r == null )
    {
      return None;
    }
    return Some(ret.r);
  }

  private function insertNode(n :Node<K,V>, key :K, val :V, ret :Ref<V>)
  {
    //do the insertion at the leaf level
    if( n == null )
    {
      ++nodeCount;
      return new Node<K,V>(key,val);
    }

    //normal BST search
    var cmp = comp(key,n.key);
    if( cmp < 0 )
    {
      n.left = insertNode(n.left,key,val,ret);
    }
    else if( cmp > 0 )
    {
      n.right = insertNode(n.right,key,val,ret);
    }
    else
    {
      //the key is already in the map, update the value
      ret.r = n.val;
      n.val = val;
    }

    return fixInvariants(n);
  }

  /**
   * Removes (\key,v) from the map if it exists.
   *
   * @return None if (\key,v) wasn't in the map, Some(v) otherwise.
   */
  public function remove(key :K) :Option<V>
  {
    var ret = new Ref<V>(null);
    if( root != null )
    {
      root = deleteNode(root,key,ret);
      if( root != null )
      {
        root.color = black;
      }
    }

    assertInvariants();

    if( ret.r == null )
    {
      return None;
    }
    return Some(ret.r);
  }

  private function deleteNode( n :Node<K,V>, key :K, ret :Ref<V> )
  {
    if( comp(key,n.key) < 0 )
    {
      if( isBlack(n.left) && isBlack(n.left.left) )
      {
        //ensure we move into a 3-node
        n = moveRedLeft(n);
      }
      n.left = deleteNode(n.left,key,ret);
    }
    else
    {
      if( isRed(n.left) )
      {
        //ensure we move into a 3-node
        n = rotateRight(n);
      }
      if( comp(key,n.key) == 0 && n.right == null )
      {
        //delete the node
        ret.r = n.val;
        --nodeCount;
        return null;
      }
      if( isBlack(n.right) && isBlack(n.right.left) )
      {
        //ensure we move into a 3-node
        n = moveRedRight(n);
      }
      if( comp(key,n.key) == 0 )
      {
        Debug.assert(n.right != null);

        ret.r = n.val;

        //ensure we are deleting a node with at most one child
        var min = minNode(n.right);
        n.val = min.val;
        n.key = min.key;
        n.right = deleteMinNode(n.right);
      }
      else
      {
        n.right = deleteNode(n.right,key,ret);
      }
    }

    return fixInvariants(n);
  }

  /** returns a view of the set of keys in this TreeMap **/
  public function keys() :SetView<K>
  {
    var _this = this;

    return {
      size: function() return _this.size(),
      iterator: function() return IterTools.mapIter(new NodeIterator(_this.root),function(x) return x.key),
      exists: function(x) {
        return switch(_this.get(x))
        {
          case None: false;
          case Some(_): true;
        };
      },
    };
  }

  /** returns a view of the collection of values in this TreeMap **/
  public function values() :CollectionView<V>
  {
    var _this = this;

    return {
      size: function() return _this.size(),
      iterator: function() return IterTools.mapIter(new NodeIterator(_this.root),function(x) return x.val),
    };
  }

  /** returns a view of the (key,value) pairs in this TreeMap **/
  public function entries() :CollectionView<Entry<K,V>>
  {
    var _this = this;

    return {
      size: function() {
        return _this.size();
      },
      iterator: function() {
        return cast new NodeIterator(_this.root);
      },
    };
  }

  /** returns the number of (key,value) pairs in the map **/
  public function size() :Int
  {
    return nodeCount;
  }

  public function toString() :String
  {
    var sb = new StringBuf();

    sb.add("{");
    for( entry in this.entries() )
    {
      sb.add("%y => %y, ".sprintf([entry.key,entry.val]));
    }
    sb.add("}");

    return sb.toString();
  }

  private static function isRed<K,V>( n :Node<K,V> )
  {
    if( n == null ) return false;
    return switch(n.color)
    {
      case red: true;
      case black: false;
    };
  }

  private static inline function isBlack<K,V>( n :Node<K,V> )
  {
    return !isRed(n);
  }

  private static function colorFlip<K,V>( n :Node<K,V> )
  {
    n.color = oppositeColor(n.color);
    n.left.color = oppositeColor(n.left.color);
    n.right.color = oppositeColor(n.right.color);
  }

  private static inline function oppositeColor( c :Color )
  {
    return switch(c)
    {
      case red: black;
      case black: red;
    };
  }

  private static function rotateLeft<K,V>( n :Node<K,V> )
  {
    Debug.assert(n != null);
    Debug.assert(n.right != null);
    /*
           n            x
          / \          / \
         a   x   =>   n   c
            / \      / \
           b   c    a   b
    */
    var x = n.right;
    n.right = x.left;
    x.left  = n;
    x.color = n.color;
    n.color = red;
    return x;
  }

  private static function rotateRight<K,V>( n :Node<K,V> )
  {
    Debug.assert( n != null );
    Debug.assert( n.left != null );
    /*
           n          x
          / \        / \
         x   c  =>  a   n
        / \            / \
       a   b          b   c
    */
    var x = n.left;
    n.left = x.right;
    x.right = n;
    x.color = n.color;
    n.color = red;
    return x;
  }

  private static function moveRedLeft<K,V>( n :Node<K,V> )
  {
    //borrow extra node from right child (which is a 3-node)
    colorFlip(n);
    if( isRed(n.right.left) )
    {
      n.right = rotateRight(n.right);
      n = rotateLeft(n);
      colorFlip(n);
    }
    return n;
  }

  private static function moveRedRight<K,V>( n :Node<K,V> )
  {
    //borrow extra node from left child (which is a 3-node)
    colorFlip(n);
    if( isRed(n.left.left) )
    {
      n = rotateRight(n);
      colorFlip(n);
    }
    return n;
  }

  private static function fixInvariants<K,V>( n :Node<K,V> )
  {
    if( isRed(n.right) && isBlack(n.left) )
    {
      //ensure left-leaning property
      n = rotateLeft(n);
    }
    if( isRed(n.left) && isRed(n.left.left) )
    {
      //balance 4-node
      n = rotateRight(n);
    }
    if( isRed(n.left) && isRed(n.right) )
    {
      //split 4-node
      colorFlip(n);
    }
    return n;
  }

  private function deleteMinNode<K,V>( n :Node<K,V> )
  {
    if( n.left == null )
    {
      //delete
      --nodeCount;
      return null;
    }

    if( isBlack(n.left) && isBlack(n.left.left) )
    {
      n = moveRedLeft(n);
    }

    n.left = deleteMinNode(n.left);

    return fixInvariants(n);
  }

  private static function minNode<K,V>( n :Node<K,V> )
  {
    Debug.assert(n != null);

    while( n.left != null )
    {
      n = n.left;
    }
    return n;
  }

  private static function maxNode<K,V>( n :Node<K,V> )
  {
    Debug.assert(n != null);

    while( n.right != null )
    {
      n = n.right;
    }
    return n;
  }

  /** Used to verify that the invariants of the tree hold **/
  private inline function assertInvariants()
  {
  #if DEBUG
    Debug.assert( isBlack(root), "root is black: " + root );

    assertIsTree(root,new List<Node<K,V>>());
    assertBlackNodeCount(root);
    assertBSTOrdering(root,comp);
  #end
  }

  private static function assertIsTree<K,V>( n: Node<K,V>, visited :List<Node<K,V>> )
  {
    if( n == null )
    {
      return;
    }

    for( r in visited )
    {
      Debug.assert( n != r );
    }
    visited.push(n);
    assertIsTree(n.left,visited);
    assertIsTree(n.right,visited);
  }

  private static function assertBlackNodeCount<K,V>( n: Node<K,V> ) :Int
  {
    if( n == null )
    {
      return 1;
    }

    var leftCount  = assertBlackNodeCount(n.left);
    var rightCount = assertBlackNodeCount(n.right);

    Debug.assert(
      leftCount == rightCount,
      "num of black nodes in all paths for left and right child not equal" + n
    );

    return leftCount + switch(n.color) {
      case red: 0;
      case black: 1;
    }
  }

  private static function assertBSTOrdering<K,V>( n: Node<K,V>, compK :K -> K -> Int ) :Void
  {
    if( n == null )
    {
      return;
    }

    if( n.left != null && n.left.val != null )
    {
      Debug.assert( compK(n.left.key,n.key) < 0, "left child not less than its parent" + n );
      assertBSTOrdering(n.left,compK);
    }

    if( n.right != null && n.right.val != null )
    {
      Debug.assert( compK(n.key,n.right.key) < 0, "parent not less than its right child" + n );
      assertBSTOrdering(n.right,compK);
    }
  }
}

private enum Color
{
  red;
  black;
}

private class Node<K,V> /*implements Entry<K,V>*/
{
  public var left   :Null<Node<K,V>>;
  public var right  :Null<Node<K,V>>;
  public var color  :Color;

  public var key :K;
  public var val :V;

  public function new(k :K, v :V)
  {
    key = k;
    val = v;
    color = red;
  }
}

private class NodeIterator<K,V>
{
  private var curr   :Node<K,V>;
  private var fringe :Array<Node<K,V>>;

  public function new( root :Node<K,V> )
  {
    fringe = new Array<Node<K,V>>();
    traverseToMin(root);
    curr = fringe.pop();
  }

  public inline function hasNext() :Bool
  {
    return curr != null;
  }

  public function next() :Node<K,V>
  {
    if( !hasNext() )
    {
      throw new NoSuchElement();
    }
    var ret = curr;

    if( fringe.length > 0 )
    {
      curr = fringe.pop();
      traverseToMin(curr.right);
    }
    else
    {
      curr = null;
    }

    return ret;
  }

  private function traverseToMin( n :Node<K,V> )
  {
    while( n != null )
    {
      fringe.push(n);
      n = n.left;
    }
  }
}