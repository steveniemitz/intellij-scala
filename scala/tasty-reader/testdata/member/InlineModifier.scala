package member

trait InlineModifier {
  inline def inlineDef: Int = ???

  transparent inline def transparentInlineDef: Int = ???
}