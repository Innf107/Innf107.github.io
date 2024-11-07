"use strict"

const termHeader = document.querySelector("#header-text");
const termCursor = document.querySelector("#term-cursor");
const termPadding = document.querySelector("#header-padding")

const timeout = (time) => new Promise((resolve, _) => setTimeout(() => resolve(), time));

const headerText = "welltypedwit.ch"
const main = async () => {
    await timeout(300)

    for (let i = 0; i < headerText.length; i++) {
        termHeader.innerText = headerText.slice(0, i + 1)
        termPadding.innerText = " ".repeat(headerText.length - i - 1)
        await timeout(50)
    }
    termCursor.innerText = ""
    termPadding.innerText = " "
}

document.addEventListener('DOMContentLoaded', () => main())
