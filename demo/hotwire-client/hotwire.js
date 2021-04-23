import * as Turbo from "@hotwired/turbo"

window.Turbo = Turbo

document.addEventListener("turbo:load", function(e) {
  console.group()
  console.log("turbo:load")
  console.log(e)
  console.groupEnd()
})

const ws = new WebSocket("ws://localhost:80");
const streamSource =
  {
    addEventListener: (_, listener) => {
      ws.onmessage = (evt) => {console.log("onmessage", evt); listener(evt)}
    },
    removeEventListener: (_, listener) => {
      ws.onclose = (evt) => {console.log("onclose", evt); listener(evt)}
    }
  }

Turbo.connectStreamSource(streamSource)
