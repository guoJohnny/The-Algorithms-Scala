package Tree
import org.scalatest.FlatSpec

class BinaryTreeSpec extends FlatSpec{
  
    "BinaryTree Serialize" should "return a serialized String passed to it" in {
    val preOrder = Array(3,9,20,15,7)
    val inOrder = Array(9,3,15,20,7)
    println(BinaryTree.serialize(BinaryTree.buildTree(preOrder,inOrder)))
    assert(BinaryTree.serialize(BinaryTree.buildTree(preOrder,inOrder)) === "3,9,null,null,20,15,null,null,7,null,null")
  }
}
