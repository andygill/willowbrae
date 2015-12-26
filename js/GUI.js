// Wrapper for the dat.gui to make menus more modular
var GUI = function(gui) {
  this.gui = gui || new dat.GUI();

  this.folder = function folder(name,code) {
    var g = new GUI(this.gui.addFolder(name))
    var r = code.call(g);
    if (r == true) {
      g.gui.open();
    }
  }

  this.add = function () { 
    return this.gui.add.apply(this.gui,arguments)
  }
}
