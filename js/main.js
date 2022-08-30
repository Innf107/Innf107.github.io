let isDarkMode = false;

function setDarkMode(darkMode){
    document.body.className = darkMode ? "dark" : "";

    const iconDark  = document.getElementById("dark-mode-dark")
    const iconLight = document.getElementById("dark-mode-light")

    iconDark.style  = darkMode ? "display: none" : ""
    iconLight.style = darkMode ? "" : "display: none"

    localStorage.setItem("color-scheme", darkMode ? "dark" : "light")
}

function toggleDarkMode(){
    isDarkMode = !isDarkMode;

    setDarkMode(isDarkMode)
}

isDarkMode = localStorage.getItem("color-scheme") == "dark"
setDarkMode(isDarkMode)

