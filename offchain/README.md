# CTL Scaffold


## Running Locally

In order to run the dApp on local, you'll need to run some backend services as well as a webpack server.

### Docker dependency

Because the backend services are run from docker via nix, you'll need to ensure your system has the latest docker installed. You can get it [here](https://www.docker.com/get-started/)

### Backend Services

Run the backend services by executing the following:

`nix run .#apps.x86_64-linux.ctl-scaffold-runtime`

You should see these backend services running:
- `ctl-server`
- `ogmios-datum-cache`
- `ogmios`
- `cardano-node`
- `postgres`

Note that `cardano-node` will take some time to sync, so you'll see some errors until it's synched.

### Webpack Server

To run the development server, enter the development shell:

`nix develop`

Then run the server:

`npm run dev`

You'll now be able to see the dApp code running in the browser at http://localhost:4008

