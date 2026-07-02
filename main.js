
const { app, BrowserWindow } = require("electron")
const { spawn } = require("child_process")
const path = require("path")
const http = require("http")

let mainWindow
let rProcess

function startR() {
  const isPackaged = app.isPackaged
  const basePath = isPackaged 
    ? path.join(process.resourcesPath)
    : __dirname
    
  const rExe = path.join(basePath, "R", "bin", "Rscript.exe")
  const rScript = path.join(basePath, "run_app.R")
  
  rProcess = spawn(rExe, [rScript], { detached: false })
  
  rProcess.stdout.on("data", (data) => console.log("R:", data.toString()))
  rProcess.stderr.on("data", (data) => console.log("R err:", data.toString()))
}

function checkServer(callback) {
  http.get("http://127.0.0.1:6865", (res) => {
    callback(true)
  }).on("error", () => {
    callback(false)
  })
}

function waitForServer() {
  checkServer((ready) => {
    if (ready) {
      mainWindow.loadURL("http://127.0.0.1:6865")
    } else {
      setTimeout(waitForServer, 2000)
    }
  })
}

function createWindow() {
  mainWindow = new BrowserWindow({
    width: 1400,
    height: 900,
    title: "MetaAnalysis",
    webPreferences: { nodeIntegration: false }
  })

  mainWindow.loadFile(path.join(__dirname, "loading.html"))
  setTimeout(waitForServer, 5000)

  mainWindow.on("closed", () => {
    if (rProcess) rProcess.kill()
    mainWindow = null
  })
}

app.on("ready", () => {
  startR()
  createWindow()
})

app.on("window-all-closed", () => {
  if (rProcess) rProcess.kill()
  app.quit()
})

