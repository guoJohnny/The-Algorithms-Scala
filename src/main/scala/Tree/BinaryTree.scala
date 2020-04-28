package Tree

import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

/**
  * The Algorithms of Binary Tree 
  * 
  */
object BinaryTree {

    def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
        preorderInorderHelper(preorder,inorder)
    }

    /**
     * 
     * Description：Given preorder and inorder traversal of a tree, construct the binary tree
     * You may assume that duplicates do not exist in the tree.
     * For example, given
     * preorder = [3,9,20,15,7]
     * inorder = [9,3,15,20,7]
     * return [3,9,20,null,null,15,7]
     * @param preorder   - a array of preorder traverse
     * @param inorder   - a array of inorder traverse
     * @return - root TreeNode
     */
    def preorderInorderHelper(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
        if (preorder.length == 0 || inorder.length == 0) return null
        var root = new TreeNode(preorder(0))
        if (preorder.length == 1) return root
        val  leftSubTreeNodeNums = inorder.indexOf(root.value)
        root.left=preorderInorderHelper(preorder.slice(1, leftSubTreeNodeNums + 1),
                inorder.slice(0, leftSubTreeNodeNums)) 
        root.right=preorderInorderHelper(preorder.slice(leftSubTreeNodeNums+1,preorder.length),
                inorder.slice(leftSubTreeNodeNums+1,inorder.length))          
        root
    }
    
    /**
     * Preorder recrusive serialize a binary tree from a root tree node
     * @param root  - a root Treenode
     * @return - serialized string of tree
     */
    def rcserialize(root: TreeNode): String = {
        if (root == null) {
            return "null"
        } 
        return root.value.toString + "," + rcserialize(root.left) + "," + rcserialize(root.right)
    }

    /**
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

}
