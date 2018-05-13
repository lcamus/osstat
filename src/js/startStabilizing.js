function startStabilizingEvent(e) {

  function sleep(x) {
    return new Promise(resolve => {
      setTimeout(() => {
        resolve(x);
      }, 2000);
    });
  }
  async function checkSelectById(network) {
    while(true) {
      console.log('calling');
      var result = await sleep('cc');
      nodeId = window.nodeSelecthtmlwidgetSelected;
      if (typeof nodeId !== 'undefined' && nodeId !== null) {
        window.nodeSelecthtmlwidgetSelected=null;
        var pos = network.getPositions([nodeId]);
        var currentScale = network.getScale();
        var targetScale = (currentScale>1.2?currentScale:1.2);
        network.moveTo({
          position: {x:pos[nodeId].x, y:pos[nodeId].y},
          scale: targetScale,
          animation: {
            duration:2000,
            easingFunction:'easeInOutCubic'
          }
        });
        network.selectNodes([nodeId],true);
        nodes=network.body.data.nodes;
        //nodeGroup=nodes.get([nodeId],{fields: ['group']})[0].group;
        nodeGroup=nodes._data[nodeId].group;
        nodeBodyHiddenColor=nodes._data[nodeId].bodyHiddenColor;
        nodes._data[nodeId].color=nodeBodyHiddenColor;
        nodes._data[nodeId].label=nodes._data[nodeId].hiddenLabel;
        nodes.update(nodes.get(nodeId));
      }
    }
  }
  checkSelectById(this);

} //startStabilizing
