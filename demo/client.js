function webSocketTest()
{
  var ws = new WebSocket("ws://localhost:80");

  ws.onopen = () => {
    ws.send("initial from js");
  };

  ws.onmessage = evt => {
    var m = evt.data;
    console.log( m );
  };

  ws.onclose = function() {
    alert("ws closed");
  };

  window.onbeforeunload = evt => {
    socket.close();
  };
}
