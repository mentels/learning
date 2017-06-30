// NOTE: The contents of this file will only be executed if
// you uncomment its entry in "assets/js/app.js".

// To use Phoenix channels, the first step is to import Socket
// and connect at the socket path in "lib/web/endpoint.ex":
import {Socket, Presence} from "phoenix"

let user = document.getElementById("User").innerText
let socket = new Socket("/socket", {params: {token: window.userToken, user: user}})

socket.connect()

// presences
let presences = {}
let formatTimestamp = (timestamp) => {
    let date = new Date(timestamp)
    return date.toLocaleTimeString()
}
let listBy = (user, {metas: metas}) => {
    return {
        user: user,
        onlineAt: formatTimestamp(metas[0].online_at)
    }
}

let userList = document.getElementById("UserList")
let render = (presences) => {
    console.log(presences)
    userList.innerHTML = Presence.list(presences, listBy)
        .map(presence => `
<li>
${presence.user}
<br>
<small>online since ${presence.onlineAt}</small>
</li>
`)
        .join("")
}

// Now that you are connected, you can join channels with a topic:
let room = socket.channel("room:lobby", {})
room.on("presence_state", state => {
    presences = Presence.syncState(presences, state)
    render(presences)
})

room.on("presence_diff", diff => {
    presences = Presence.syncDiff(presences, diff)
    render(presences)
})

room.join()

let messageInput = document.getElementById("NewMessage")
messageInput.addEventListener("keypress", (e) => {
    if (e.keyCode == 13 && messageInput.value != "") {
        room.push("message:new", messageInput.value)
        messageInput.value = ""
    }
})

let messageList = document.getElementById("MessageList")
let renderMessage = (message) => {
    let messageElement = document.createElement("li")
    messageElement.innerHTML = `
<b>${message.user}</b>
<i>${formatTimestamp(message.timestamp)}</i>
<p>${message.body}</p>
`
    messageList.appendChild(messageElement)
    messageList.scrollTop = messageList.scrollHeight
}
room.on("message:new", message => renderMessage(message))

export default socket
