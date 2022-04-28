package org.jetbrains.plugins.scala.tasty

import dotty.tools.tasty.TastyBuffer.{Addr, NameRef}
import dotty.tools.tasty.TastyFormat._
import dotty.tools.tasty.{TastyReader, UnpickleException}

private class TreeReader(nameAtRef: NameTable) {
  private def readNat(in: TastyReader): Int = in.readNat()

  private def nameToString(name: Name): String = name.toString

  private def nameRefToString(ref: NameRef): String = nameToString(nameAtRef(ref))

  private def readName(in: TastyReader): String = {
    val idx = in.readNat()
    nameRefToString(NameRef(idx))
  }

  private def readTree(in: TastyReader): Node = {
    val addr = in.currentAddr

    val tag = in.readByte()

    var nat = -1

    var value = -1L

    var names = Seq.empty[String]
    var children: () => Seq[Node] = () => Seq.empty

    if (tag >= firstLengthTreeTag) {
      val len = in.readNat()
      val end = in.currentAddr + len

      def treeReader(): () => Seq[Node] = {
        val addr = in.currentAddr
        () => {
          in.goto(addr)
          val trees = in.until(end)(readTree(in))
          trees.zip(trees.drop(1)).foreach { case (a, b) =>
            a.nextSibling = Some(b)
            b.previousSibling = Some(a)
          }
          trees
        }
      }

      tag match {
        case RENAMED =>
//          names :+= readName(in); names :+= readName(in)
        case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | NAMEDARG | BIND =>
          names :+= readName(in); children = treeReader()
        case REFINEDtype | TERMREFin | TYPEREFin | SELECTin =>
          names :+= readName(in); children = treeReader()
        case RETURN | HOLE =>
//          readNat(in); children :++= readTrees()
        case METHODtype | POLYtype | TYPELAMBDAtype =>
//          children :+= readTree(in)
//          while (in.currentAddr.index < end.index && !isModifierTag(in.nextByte)) { children :+= readTree(in); names :+= readName(in); }
//          children :++= readTrees()
        case PARAMtype =>
//          readNat(in); readNat(in)
        case _ =>
          children = treeReader()
      }
      in.goto(end)
    }
    else if (tag >= firstNatASTTreeTag) {
      tag match {
        case IDENT | IDENTtpt | SELECT | SELECTtpt | TERMREF | TYPEREF | SELFDEF => names :+= readName(in)
        case _ => nat = readNat(in)
      }
      children = { val tree = readTree(in); () => Seq(tree) }
    }
    else if (tag >= firstASTTreeTag)
      children = { val tree = readTree(in); () => Seq(tree) }
    else if (tag >= firstNatTreeTag)
      tag match {
        case TERMREFpkg | TYPEREFpkg | STRINGconst | IMPORTED => names :+= readName(in)
        case CHARconst => value = in.readNat()
        case BYTEconst | SHORTconst | INTconst | FLOATconst => value = in.readInt()
        case LONGconst | DOUBLEconst => value = in.readLongInt()
        case _ => nat = readNat(in)
      }

    tag match {
      case SHAREDtype =>
        val node = readTree(in.subReader(Addr(nat), in.endAddr)) // TODO cache?
        node.isSharedType = true
        node
      case SHAREDterm => readTree(in.subReader(Addr(nat), in.endAddr)) // TODO cache?
      case _ =>
        val node = new Node(addr, tag, names, children)
        tag match {
          case TYPEREFsymbol | TYPEREFdirect | TERMREFsymbol | TERMREFdirect =>
            val in0 = in.subReader(Addr(nat), in.endAddr)
            in0.readByte() // Tag
            in0.readNat() // Length
            node.refName = Some(readName(in0)) // TODO use as node name?
          case _ =>
        }
        node.value = value
        node
    }
  }
}

object TreeReader {
  def treeFrom(bytes: Array[Byte]): Node = {
    val in = new TastyReader(bytes)

    new HeaderReader(in).readFullHeader()

    val nameTableReader = new NameTableReader(in)
    nameTableReader.read()
    val nameTable = nameTableReader.nameAtRef

    val sectionName = nameTable.apply(NameRef(in.readNat()))
    val sectionEnd = in.readEnd()
    if (sectionName.asSimpleName.toString != "ASTs") {
      throw new UnpickleException("No ASTs section")
    }
    new TreeReader(nameTable).readTree(new TastyReader(bytes, in.currentAddr.index, sectionEnd.index, in.currentAddr.index))
  }
}
