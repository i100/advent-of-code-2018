import scala.io.Source 
import scala.util.{ Try }


object Tree {
  case class Node(pointer: Int, children: List[Node], metadata: List[Int])

  object SerialParser {
    abstract class Token() { val valueOf: Int }
    case class ChildrenHeader(val valueOf: Int) extends Token 
    case class MetadataHeader(val valueOf: Int) extends Token 
    case class Metadata(val valueOf: Int) extends Token

    case class OpenNode(pointer: Int, numChildren: Int, numMetadata: Int)
    
    // returns a tuple containing the root node and the lexed input
    def parse(serialTree: List[Int]) = {
      val elCount = serialTree.length

      // We use two stacks, one to keep track of openNodes, i.e. nodes whose definition 
      // hasn't been fully read from the input yet; and a nodeStack, which tracks nodes 
      // that haven't yet been assigned as children. unclaimedMetadatas holds all the metas
      // which haven't yet been assigned to a completed node. 
      def nextToken(raw: List[Int], procdTokens: List[Token], 
            openNodes: List[OpenNode], nodeStack: List[Node], 
            unclaimedMetadatas: List[Int]): (Node, List[Token]) = {
        
        procdTokens match {
          case Nil => 
            nextToken(raw.tail, ChildrenHeader(raw.head) :: procdTokens, 
              openNodes, nodeStack, unclaimedMetadatas)
          case ChildrenHeader(d) :: t =>
            if (raw.head == 0) throw new Exception("Malformed input, MetadataHeader must be > 0")
            nextToken(raw.tail, MetadataHeader(raw.head) :: procdTokens, 
              OpenNode(elCount - raw.length, d, raw.head) :: openNodes,
              nodeStack, unclaimedMetadatas)
          case MetadataHeader(d) :: t =>
            if (openNodes(0).numChildren == 0) {
              nextToken(raw.tail, Metadata(raw.head) :: procdTokens, openNodes, 
                nodeStack, raw.head :: unclaimedMetadatas) 
            } else {
              nextToken(raw.tail, ChildrenHeader(raw.head) :: procdTokens, openNodes, 
                nodeStack, unclaimedMetadatas)
            }
          case Metadata(d) :: t =>
            val metadatasCompleted =  openNodes(0).numMetadata == unclaimedMetadatas.length
            val completesNode = if (metadatasCompleted) true else false 

            if (! completesNode) {
              nextToken(raw.tail, Metadata(raw.head) :: procdTokens, openNodes, nodeStack, 
                raw.head :: unclaimedMetadatas)
            } else {
              val thisNode = openNodes(0)
              val completedNode = Node(thisNode.pointer, nodeStack.take(thisNode.numChildren), 
                unclaimedMetadatas)
              val updatedNodeStack = completedNode :: nodeStack.drop(thisNode.numChildren)
              val updatedOpenNodes = openNodes.drop(1)
              val numChildrenOfParent = Try(updatedOpenNodes(0).numChildren).getOrElse(-1)
              val parentPointer = Try(updatedOpenNodes(0).pointer).getOrElse(-1)  

              if (! raw.isEmpty) {
                val parentsChildrenCompleted: Boolean = updatedNodeStack
                  .filter(_.pointer > parentPointer)
                  .length equals numChildrenOfParent

                if (parentsChildrenCompleted) {
                  nextToken(raw.tail, Metadata(raw.head) :: procdTokens,
                    updatedOpenNodes, updatedNodeStack, 
                    List(raw.head))
                } else {
                  nextToken(raw.tail, ChildrenHeader(raw.head) :: procdTokens,
                    updatedOpenNodes, updatedNodeStack, 
                    List())
                }
              } else { // return 
                (completedNode, procdTokens.reverse)
              }
            }
          case h :: t => throw new Exception("Unknown token")
        }
      }
      nextToken(serialTree, List(), List(), List(), List())
    }
  }

  /*
  def search(root: Node, by: ..., global = true) = {

  }

  def traverse(root: Node): List[Node] = {

  }
  */
}


val rawTree: List[Int] = Source.fromFile("./input")
  .mkString.trim.split(" ").toList.map(_.toInt)

val (root, lexedInput) = Tree.SerialParser.parse(rawTree)

val DO_PART_1 = true
val DO_PART_2 = true 


if (DO_PART_1) {
  val metadataSum = lexedInput.filter { 
    case Tree.SerialParser.Metadata(d) => true 
    case _ => false 
  }.map(_.valueOf).sum

  println(s"Metadata sum: $metadataSum")
}


if (DO_PART_2) {

}
