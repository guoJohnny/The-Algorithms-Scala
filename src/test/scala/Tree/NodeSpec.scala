package Tree
import org.scalatest.FlatSpec

class NodeSpec extends FlatSpec{
   
    val preOrder = Array(3,9,20,15,7)
    val inOrder = Array(9,3,15,20,7)
    val postOrder = Array(9,15,7,20,3)
    val root = BinaryTree.buildFromPreIn(preOrder,inOrder)

    var obj = new Codec()
   
    "BinaryTree rcSerialize" should "return a serialized String passed to it" in {
    assert(obj.rcserialize(root) === "[3,9,null,null,20,15,null,null,7,null,null]")
  }

  "BinaryTree rcDeserialize" should "return a deserialized Treenode passed to it" in {
    assert(obj.serialize(obj.preorderDeserialize("[1,2,null,null,3,4,null,null,5,null,null]")) === "[1,2,3,null,null,4,5]")
  }

  "BinaryTree Recrusively Serialize and Deserialize" should "passed to the case" in {
    val preOrder = Array(1,2,3,4,5)
    val inOrder = Array(2,1,4,3,5)
    val s = obj.rcserialize(BinaryTree.buildFromPreIn(preOrder,inOrder))
    assert(s === "[1,2,null,null,3,4,null,null,5,null,null]")
    val ans = obj.preorderDeserialize(s)
    assert(obj.serialize(ans) === "[1,2,3,null,null,4,5]")
  }

  "BinaryTree serialize" should "return a serialized Treenode passed to it" in {
    val preOrder = Array(1,2,3,4,5)
    val inOrder = Array(2,1,4,3,5)
    val s = obj.serialize(BinaryTree.buildFromPreIn(preOrder,inOrder))
    assert(s === "[1,2,3,null,null,4,5]")
  }

  "BinaryTree deserialize" should "return a serialized Treenode passed to it" in {
    assert(obj.serialize(obj.deserialize("[1,2,3,null,null,4,5]")) === "[1,2,3,null,null,4,5]")
  }

  "BinaryTree Iteratively Serialize and Deserialize" should "passed to the case" in {
    val preOrder = Array(1,2,3,4,5)
    val inOrder = Array(2,1,4,3,5)
    val s = obj.serialize(BinaryTree.buildFromPreIn(preOrder,inOrder))
    assert(s === "[1,2,3,null,null,4,5]")
    val ans = obj.deserialize(s)
    assert(obj.serialize(ans) === "[1,2,3,null,null,4,5]")
  }
}
