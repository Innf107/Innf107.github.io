import * as React from 'react'
import satori from 'satori'
import { Resvg } from '@resvg/resvg-js'
import * as fs from 'fs'

const title = process.argv[2];
const date = process.argv[3];

(async () => {
  const primaryStart = "rgb(0, 140, 255)"
  const primaryEnd = "rgb(255, 81, 255)"
  const gradient = `linear-gradient(to right, ${primaryStart}, ${primaryEnd})`
  const svg = await satori(<div

    style={{
      height: '100%',
      width: '100%',
      display: 'flex',
      flexDirection: 'column',
      alignItems: 'flex-start',
      justifyContent: 'center',
      backgroundColor: '#000',
      color: "white",
      fontSize: 32,
      fontWeight: 600,
      fontFamily: 'Inria Sans',
    }}
  >
    <div style={{ position: "absolute", right: 10, top: 10, fontWeight: 700, fontSize: 25, background: gradient, color: "transparent", backgroundClip: "text" }}>welltypedwit.ch</div>
    <div style={{ display: "flex", background: gradient }}>
      <div style={{ marginBottom: "3px", backgroundColor: "black" }}>{title}</div>
    </div>
    <div style={{ color: "rgb(150, 150, 150)", fontWeight: "normal", fontSize: 20, alignSelf: "flex-start" }}>{date}</div>
  </div>, {
    width: 600, height: 400, fonts: [
      {
        name: "Inria Sans",
        data: fs.readFileSync("./opengraph/fonts/InriaSans-Regular.ttf"),
        weight: 400
      },
      {
        name: "Inria Sans",
        data: fs.readFileSync("./opengraph/fonts/InriaSans-Bold.ttf"),
        weight: 700
      }
    ]
  })

  fs.writeSync(process.stdout.fd, new Resvg(svg).render().asPng())
})()