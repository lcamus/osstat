$( document ).ready(function() {

  var table0 = $('#DataTables_Table_0').DataTable();
  var table1 = $('#DataTables_Table_1').DataTable();

  var fakesearch='non existing node';
  table0.search(fakesearch,false,false,false).draw();
  // $('#DataTables_Table_0 caption').text('Incoming '+e.node);
  table1.search(fakesearch,false,false,false).draw();
  // $('#DataTables_Table_1 caption').text('Outcoming '+e.node);

  //
  function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
  async function delayfit() {
    await sleep(5000);
    window.network.fit({animation: true});
  }
  delayfit();
  //

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

    table0.search(selectedNode,false,false,false).draw();
    $('#DataTables_Table_0 caption').text('Incoming '+selectedNode);
    table1.search(selectedNode,false,false,false).draw();
    $('#DataTables_Table_1 caption').text('Outcoming '+selectedNode);

  }//manageSelectById

  var selectById = document.querySelector('[id^=\"nodeSelecthtmlwidget-\"]');

  ['onchange','change'].forEach(e =>
    selectById.addEventListener(e,function(){manageSelectById(this.value);},false)
  );

  // s=$("select#nodeSelecthtmlwidget-c70f78b0512ba786ba64 option").map(function() {return $(this).val();}).get();
  // s=s.forEach(i => console.log(/^\/\w+\//.exec(i)));

});
