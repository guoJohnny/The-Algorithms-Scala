package Tree
import scala.Array
import org.scalatest.FlatSpec

class BinaryTreeSpec extends FlatSpec{

  val preOrder = Array(3,9,20,15,7)
  val inOrder = Array(9,3,15,20,7)
  val postOrder = Array(9,15,7,20,3)
  val root = BinaryTree.buildFromPreIn(preOrder,inOrder)

  var obj = new Codec()
  
  "BinaryTree inorder prester build tree" should "return a String passed to it" in {  
    assert(obj.serialize(root) === "[3,9,20,null,null,15,7]")
  }

  "BinaryTree inorder poster build tree" should "return a String passed to it" in {  
    assert(obj.serialize(BinaryTree.buildFromInPost(inOrder,postOrder)) === "[3,9,20,null,null,15,7]")
  }

  "BinaryTree unique BST's number" should "return a Int passed to it" in {  
    assert(BinaryTree.numTrees(3) === 5)
  }

  "BinaryTree unique BST" should "return a List passed to it" in { 
    val list = for {node <- BinaryTree.generateTrees(3)} yield obj.serialize(node)
    assert(list.mkString("[",",","]") === "[[1,null,2,null,3],[1,null,3,2],[2,1,3],[3,1,null,null,2],[3,2,null,1]]")
  }

  "BinaryTree valid BST" should "return a Boolean passed to it" in { 
    assert(BinaryTree.isValidBST(root) === false)
    assert(BinaryTree.inorderValidBST(root) === false)
    val preOrder = Array[Int](2,1,3)
    val inOrder = Array[Int](1,2,3)
    assert(BinaryTree.isValidBST(BinaryTree.buildFromPreIn(preOrder,inOrder)) === true)
    assert(BinaryTree.inorderValidBST(BinaryTree.buildFromPreIn(preOrder,inOrder)) === true)
  }

  "BinaryTree recover BST" should "return a recovered tree passed to it" in { 
    val preOrder = Array[Int](1, 3, 2)
    val inOrder = Array[Int](3, 2, 1)
    val root = BinaryTree.buildFromPreIn(preOrder,inOrder)
    val rcRoot = BinaryTree.buildFromPreIn(preOrder,inOrder)
    BinaryTree.recoverTree(root)
    BinaryTree.rcRecoverTree(rcRoot)
    assert(obj.serialize(root) === "[3,1,null,null,2]")
    assert(obj.serialize(rcRoot) === "[3,1,null,null,2]")
  }

  "BinaryTree level traversal" should "return a List" in {  
    val tree: List[List[Int]] =
        List(
            List(3),
            List(9,20),
            List(15,7)
        )
    assert(BinaryTree.levelOrder(root) === tree)
  }

  "BinaryTree recursive max depth" should "return a Int passed to it" in {
    assert(BinaryTree.rcmaxDepth(root) === 3)
  }

  "BinaryTree max depth" should "return a Int passed to it" in {
    assert(BinaryTree.rcmaxDepth(root) === 3)
  }

  "BinaryTree recursive min depth" should "return a Int passed to it" in {
    assert(BinaryTree.rcMinDepth(root) === 2)
  }

  "BinaryTree BFS min depth" should "return a Int passed to it" in {
    assert(BinaryTree.bfsMinDepth(root) === 2)
  }

  "Recrusively Symmetric Tree" should "return a boolean passed to it" in {
    assert(BinaryTree.rcSymmetric(obj.deserialize("[1,2,2]")) === true)
  }

  "Iteratively Symmetric Tree" should "return a boolean passed to it" in {
    assert(BinaryTree.isSymmetric(obj.deserialize("[1,2,2]")) === true)
  }

  "zigzag level order" should "return a List" in {  
    val tree: List[List[Int]] =
        List(
            List(3),
            List(20,9),
            List(15,7)
        )
    assert(BinaryTree.zigzagLevelOrder(root) === tree)
  }

  "BinaryTree rcInorder" should "return a List passed to it" in {
    val list = List[Int](1,3,2)
    val preOrder = Array(1,2,3)
    val inOrder = Array(1,3,2)
    assert(BinaryTree.rcInorderTraversal(BinaryTree.buildFromPreIn(preOrder, inOrder)) === list)
  }

  "BinaryTree Inorder" should "return a List passed to it" in {
    val list = List[Int](1,3,2)
    val preOrder = Array(1,2,3)
    val inOrder = Array(1,3,2)
    assert(BinaryTree.inorderTraversal(BinaryTree.buildFromPreIn(preOrder,inOrder)) === list)
  }

  "BinaryTree postorder traversal" should "return a List" in {  
    val tree = List[Int](3,2,1)
    val preOrder = Array[Int](1, 2, 3)
    val inOrder = Array[Int](3, 2, 1)
    val rcRoot = BinaryTree.buildFromPreIn(preOrder,inOrder)
    val root = BinaryTree.buildFromPreIn(preOrder,inOrder)
    assert(BinaryTree.rcPostorderTraversal(rcRoot) === tree)
    assert(BinaryTree.postorderTraversal(root) === tree )
  }

  "BinaryTree isBalanced" should "return a Boolean passed to it" in {
    assert(BinaryTree.isBalanced(BinaryTree.buildFromPreIn(preOrder,inOrder)) === true)
  }

  "Build binaryTree from a sort array" should "return a Tree passed to it" in {  
    assert(obj.serialize(BinaryTree.sortedArrayToBST(Array[Int](-10,-3,0,5,9))) === "[0,-10,5,null,-3,null,9]")
  }

  "Build binaryTree from a sort list" should "return a Tree passed to it" in { 
    // convert array to list [-10,-3,0,5,9]
    def setListNode(array: Array[Int]): ListNode = {
        val nullHead:ListNode = new ListNode
        array.foldLeft(nullHead)((node:ListNode, num:Int) => {
            val item = new ListNode(num)
            node.next = item
            item
        })
        nullHead.next
    }
    assert(obj.serialize(BinaryTree.sortedListToBST(setListNode(Array[Int](-10,-3,0,5,9)))) === "[0,-10,5,null,-3,null,9]")
  }

  "BinaryTree flatten" should "return a flatten serialize list passed to it" in {
    val rcRoot = obj.deserialize("[1,2,5,3,4,null,6]")
    val root = obj.deserialize("[1,2,5,3,4,null,6]")
    BinaryTree.rcFlatten(rcRoot)
    BinaryTree.flatten(root)    
    assert(obj.serialize(rcRoot) === "[1,null,2,null,3,null,4,null,5,null,6]")
    assert(obj.serialize(root) === "[1,null,2,null,3,null,4,null,5,null,6]")
  }

  "Find binaryTree math path sum" should "return a int passed to it" in {
    val root = obj.deserialize("[-10,9,20,null,null,15,7]")
    assert(BinaryTree.maxPathSum(root) === 42)
  }

  "Binary Tree Right Side View" should "return a list passed to it" in {
    val root = obj.deserialize("[1,2,3,4]")
    assert(BinaryTree.rightSideView(root).mkString("[",",","]") === "[1,3,4]")
  }

  "Count Complete Tree Nodes" should "return a number passed to it" in {
    val root = obj.deserialize("[1,2,3,4,5,6]")
    assert(BinaryTree.countNodes(root) === 6)
  }

  "Invert Binary Tree" should "return a new tree passed to it" in {
    val root = obj.deserialize("[4,2,7,1,3,6,9]")
    assert(obj.serialize(BinaryTree.invertTree(root)) === "[4,7,2,9,6,3,1]")
    val root_new = obj.deserialize("[4,2,7,1,3,6,9]")    
    assert(obj.serialize(BinaryTree.iterInvertTree(root_new)) === "[4,7,2,9,6,3,1]")
  }
  
  "Lowest Common Ancestor of a Binary Search Tree" should "return a number passed to it" in {
    val root = obj.deserialize("[6,2,8,0,4,7,9,null,null,3,5]")
    val p = new TreeNode(2)
    val q = new TreeNode(8)
    assert(BinaryTree.lowestCommonAncestor(root, p, q).value === 6)
    assert(BinaryTree.iterLowestCommonAncestor(root, p, q).value === 6)
  }

  "Lowest Common Ancestor of a Binary tree " should "return a number passed to it" in {
    val root = obj.deserialize("[3,5,1,6,2,0,8,null,null,7,4]")
    val p = new TreeNode(5)
    val q = new TreeNode(1)
    assert(BinaryTree.lowestCommonAncestorBT(root, p, q).value === 3)
  }

  "Convert BST to Greater Tree" should "return a new Tree passed to it" in {
    val root = obj.deserialize("[4,1,6,0,2,5,7,null,null,null,3,null,null,null,8]")
    assert(obj.serialize(BinaryTree.bstToGst(root)) === "[30,36,21,36,35,26,15,null,null,null,33,null,null,null,8]")
  }

  "Balance a Binary Search Tree" should "return a new Treenode passed to it" in {
    val root = obj.deserialize("[1,null,2,null,3,null,4,null,null]")
    assert(obj.serialize(BinaryTree.balanceBST(root)) === "[2,1,3,null,null,null,4]")
  }
}
