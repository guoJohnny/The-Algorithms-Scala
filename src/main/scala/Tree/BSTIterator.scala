package Tree
import scala.collection.mutable.ArrayBuffer

/**
  * Leetcode 173
  * Implement an iterator over a binary search tree (BST).
  * Your iterator will be initialized with the root node of a BST.
  * Calling next() will return the next smallest number in the BST.
  * @param _root
  */
class BSTIterator(_root: TreeNode) {
    val root = _root
    val array = {
        val array = ArrayBuffer[Int]()
        def helper(root: TreeNode): Unit = {
            if (root == null) return 
            if (root.left != null) helper(root.left)
            array.append(root.value)
            if (root.right != null) helper(root.right)
        }
        helper(root)
        array
    }
    var index = -1
    /** @return the next smallest number */
    def next(): Int = {
        index += 1
        array(index)
    }

    /** @return whether we have a next smallest number */
    def hasNext(): Boolean = {
        if (index + 1 <= (array.length - 1)) return true
        false
    }
}

class BSTStackIterator(_root: TreeNode) {
    var stack = List[TreeNode]()

    push(_root)

    def push(root: TreeNode) : Unit = {
        var tmp = root
        while (tmp != null) {
            stack = tmp :: stack
            tmp = tmp.left
        }
    }
    
    /** @return the next smallest number */
    def next(): Int = {
        val curNode = stack.head
        stack = stack.tail
        if (curNode.right != null) {
            push(curNode.right)
        }
        return curNode.value
    }

    /** @return whether we have a next smallest number */
    def hasNext(): Boolean = {
        stack.length != 0
    }
}