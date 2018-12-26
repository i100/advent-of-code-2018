import scala.io.Source 
import scala.util.{ Try, Success, Failure }


case class Node(pointer: Int, children: List[Node], metadata: List[Int])

object Tree {

  object SerialParser {
    abstract class Token() { val valueOf: Int }
    case class ChildrenHeader(val valueOf: Int) extends Token 
    case class MetadataHeader(val valueOf: Int) extends Token 
    case class Metadata(val valueOf: Int) extends Token

    case class OpenNode(pointer: Int, numChildren: Int, numMetadata: Int)
    
    // returns a 2-tuple containing the root node and the lexed input
    def parse(serialTree: List[Int]) = {
      val elCount = serialTree.length

      // We use two stacks, one to keep track of openNodes, i.e. nodes whose definition 
      // hasn't been fully read from the input yet; and unclaimedNodes, which tracks nodes 
      // that haven't yet been assigned as children. unclaimedMetadata holds all the metas
      // which haven't yet been assigned to a completed node. 
      def nextToken(raw: List[Int], procdTokens: List[Token], 
            openNodes: List[OpenNode], unclaimedNodes: List[Node], 
            unclaimedMetadata: List[Int]): (Node, List[Token]) = {
        
        procdTokens match {
          case Nil => 
            nextToken(raw.tail, ChildrenHeader(raw.head) :: procdTokens, 
              openNodes, unclaimedNodes, unclaimedMetadata)
          case ChildrenHeader(d) :: t =>
            if (raw.head == 0) throw new Exception("Malformed input, MetadataHeader must be > 0")
            nextToken(raw.tail, MetadataHeader(raw.head) :: procdTokens, 
              OpenNode(elCount - raw.length, d, raw.head) :: openNodes,
              unclaimedNodes, unclaimedMetadata)
          case MetadataHeader(d) :: t =>
            if (openNodes(0).numChildren == 0) {
              nextToken(raw.tail, Metadata(raw.head) :: procdTokens, openNodes, 
                unclaimedNodes, raw.head :: unclaimedMetadata) 
            } else {
              nextToken(raw.tail, ChildrenHeader(raw.head) :: procdTokens, openNodes, 
                unclaimedNodes, unclaimedMetadata)
            }
          case Metadata(d) :: t =>
            val metadataCompleted =  openNodes(0).numMetadata == unclaimedMetadata.length
            val completesNode = if (metadataCompleted) true else false 

            if (! completesNode) {
              nextToken(raw.tail, Metadata(raw.head) :: procdTokens, openNodes, unclaimedNodes, 
                raw.head :: unclaimedMetadata)
            } else {
              val thisNode = openNodes(0)
              val completedNode = Node(
                thisNode.pointer, 
                unclaimedNodes.take(thisNode.numChildren).reverse, 
                unclaimedMetadata.reverse)
              val updatedNodeStack = completedNode :: unclaimedNodes.drop(thisNode.numChildren)
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
}

class Tree(root: Node) {
  // returns the nodes in lexicographc order -- i.e. from leaves to root
  def traverse(): List[Node] = {
    def doTraverse(layer: List[Node], discoveredNodes: List[Node]): List[Node] = {
      val children = layer.flatMap(_.children)
      val childCount = children.length
      
      if (childCount > 0) {
        doTraverse(children, layer ::: discoveredNodes)
      } else {
        layer ::: discoveredNodes 
      }
    }
   doTraverse(List(root), List())
  }
}

val rawTree: List[Int] = Source.fromFile("./input")
  .mkString.trim.split(" ").toList.map(_.toInt)

val (root, lexedInput) = Tree.SerialParser.parse(rawTree)

val DoPart1 = true
val DoPart2 = true 


if (DoPart1) {
  val metadataSum = lexedInput.filter { 
    case Tree.SerialParser.Metadata(d) => true 
    case _ => false 
  }.map(_.valueOf).sum

  println(s"Metadata sum: $metadataSum")
}


if (DoPart2) {
  val tree = new Tree(root)
  val sortedNodes = tree.traverse()

  def calcNodeValues() = {
    def doCalc(nodes: List[Node], procdNodes: List[(Node, Int)]): List[(Node, Int)] = { 
      nodes match {
        case Nil => procdNodes.reverse
        case head :: tail if head.children.length == 0 => 
          doCalc(tail, (head, head.metadata.sum) :: procdNodes)
        case head :: tail =>
          val childPointers: List[Int] = head.metadata.filter { i =>
            Try(head.children(i - 1)) match { 
              case Success(n) => true 
              case Failure(e) => false
            }
          }.map { i => head.children(i - 1).pointer }
          // this is an ugly solution, it would be nicer to double link the
          // nodes
          val nodeValue = childPointers.map { p => 
            procdNodes.view.filter(_._1.pointer == p).head._2
          }.sum

          doCalc(tail, (head, nodeValue) :: procdNodes)
      }
    }

    doCalc(sortedNodes, List())
  }
  
  val nodeValues = calcNodeValues()
  val rootValue = nodeValues.last._2
  
  println(s"Root node value: $rootValue")
}
