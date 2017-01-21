module Dhall.Xml where

import Dhall.Core (Expr(..), 
import qualified Text.XmlHtml as XH

nodeToExpr :: XH.Node -> Maybe (Expr s a)
nodeToExpr x = case x of
  XH.TextNode t -> Just (ExprXmlNode (NodeText t))
  XH.Comment _ -> Nothing
  XH.Element tag attrs children ->
    let exprChildren = mapMaybe nodeToExpr children
     in ExprXmlNode exprChildren

