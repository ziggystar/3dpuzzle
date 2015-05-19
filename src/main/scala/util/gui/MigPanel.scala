package util.gui

import net.miginfocom.swing.MigLayout

import scala.swing.{Component, Panel, SequentialContainer}

class MigPanel(layoutConstrains: String,
               colConstriants: String,
               rowConstraints: String)
  extends Panel with SequentialContainer.Wrapper
{
  override lazy val peer = new javax.swing.JPanel(new MigLayout(layoutConstrains, colConstriants, rowConstraints))

  def this(layoutConstraints: String) = this (layoutConstraints, "", "")

  private def layoutManager = peer.getLayout.asInstanceOf[MigLayout]

  protected def add(c: Component, l: String) {peer.add(c.peer, l)}

  protected def add(c: Component) {peer.add(c.peer)}
}
