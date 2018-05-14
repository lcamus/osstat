$( document ).ready(function() {

  function manageSelectById(selectedNode) {

    var network=window.network
    console.log('node selected by id: '+selectedNode);

    var pos = network.getPositions([selectedNode]);
    var currentScale = network.getScale();
    network.moveTo({
      position: {x:pos[selectedNode].x, y:pos[selectedNode].y},
      scale: (currentScale>1.2?currentScale:1.2),
      animation: {
        duration:2000,
        easingFunction:'easeInOutCubic'
      }
    });//moveTo

  }//manageSelectById

  var selectById = document.querySelector('[id^=\"nodeSelecthtmlwidget-\"]');
  // selectById.addEventListener('onchange',function(){manageSelectById(this.value);},false);
  ['onchange','change'].forEach(e =>
    selectById.addEventListener(e,function(){manageSelectById(this.value);},false)
  );

});
