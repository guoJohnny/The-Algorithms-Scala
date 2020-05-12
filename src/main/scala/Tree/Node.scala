package Tree

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

/**
  * Definition for a binary tree node.
  * @param _value
  */
class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
}
/**
  * Definition for a list node.
  * @param _x
  */
class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
}

/**
 * Your Codec object will be instantiated and called as such:
 * var obj = new Codec()
 * val s = obj.serialize(root)
 * val ans = obj.deserialize(s)
 */
class Codec {
    // Encodes a list of strings to a single string.
    /**
     * Leetcode 297
     * Preorder recrusive serialize a binary tree from a root tree node
     * @param root  - a root Treenode
     * @return - serialized string of tree
     */
    def rcserialize(root: TreeNode): String = {
        def helper(root: TreeNode): String = {
            if (root == null) return "null"
            return root.value.toString + "," + helper(root.left) + "," + helper(root.right) 
        }
        return "[" + helper(root) + "]"
    }

    /** 
      * Leetcode 297
      * Description：Level order traversal serialize
      * preorder as Array(1,2,3,4,5) and inorder as Array(2,1,4,3,5)
      * from preorder and inorder build a binary tree,serialize this tree
      * return [1,2,3,null,null,4,5]
      * @param root - a root Treenode
      * @return - serialized string of tree
      */
    def serialize(root: TreeNode): String = {
        val queue = new Queue[TreeNode]
        val str = ArrayBuffer.empty[String]
        queue.enqueue(root)
        while (!queue.isEmpty){
            val curNode = queue.dequeue
            if (curNode != null){                
                str += curNode.value.toString
                queue.enqueue(curNode.left)
                queue.enqueue(curNode.right)
            } else {
                str += null
            }
        }
        // remove tail "null" nodes
        str.reverse.dropWhile( {x:String => x == null} ).reverse.mkString("[", ",", "]")
    }
    
    // Decodes a single string to a list of strings.
    def preorderDeserialize(s: String): TreeNode = {
        var root: TreeNode = null
        if (s.isEmpty() || s.length() <= 2) return root
        val data = s.substring(1, s.length() - 1).split(",")
        var level = 0
        def helper(): TreeNode = {
            if (data(level).equals("null")) {
                level += 1  
                return null
            } 
            var cur = new TreeNode(data(level).trim().toInt)   
            level += 1                 
            cur.left = helper()
            cur.right = helper()
            cur
        }
        helper()
    }

    def deserialize(s: String): TreeNode = {
        var root: TreeNode = null
        var preLevel = List[TreeNode]()
        if (s.isEmpty() || s.length() <= 2) return root
        val data = s.substring(1, s.length() - 1).split(",")
        root = new TreeNode(data(0).toInt)
        preLevel = preLevel :+ root
        var idx = 1
        while (preLevel.nonEmpty){
            var curLevel = List[TreeNode]()
            preLevel.foreach(parent => {
                if (idx < data.length && !data(idx).equals("null")) {
                    val left = new TreeNode(data(idx).toInt)
                    parent.left = left
                    curLevel = curLevel :+ left
                }
                if (idx + 1 < data.length && !data(idx + 1).equals("null")) {
                    val right = new TreeNode(data(idx + 1).toInt)
                    parent.right = right
                    curLevel = curLevel :+ right
                }
                idx += 2
            })
            preLevel = curLevel
        }
        root
    }
}