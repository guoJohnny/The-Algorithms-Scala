package Tree

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
     * Recrusive serialize a binary tree from a root tree node
     * @param root  - a root Treenode
     * @param serial    - serialized string of tree
     * @return - serialized string of tree
     */
    def serialize(root: TreeNode, serial: String): String = {
        var tmp = serial
        if (root == null) {
            tmp = tmp + "null,"
        } else {
            tmp = tmp + root.value.toString.concat(",")
            tmp = serialize(root.left, tmp)
            tmp = serialize(root.right, tmp)
        }
        return tmp
    }

}
