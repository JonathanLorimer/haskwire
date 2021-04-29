import * as Turbo from "@hotwired/turbo"

window.Turbo = Turbo

document.addEventListener("turbo:load", function(e) {
  console.group()
  console.log("turbo:load")
  console.log(e)
  console.groupEnd()
})

const ws = new WebSocket("ws://localhost:8081");
const streamSource =
  {
    addEventListener: (_, listener) => {
      ws.onmessage = listener
    },
    removeEventListener: (_, listener) => {
      ws.onclose = listener
    }
  }

Turbo.connectStreamSource(streamSource)
