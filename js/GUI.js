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
  this.addColor = function () { 
    return this.gui.addColor.apply(this.gui,arguments)
  }

  //gui.variable('floor.position',floor.position,'x',0.5)
  this.variable = function(name,rhs,field) {
      this.add(rhs, field)
  }
}
