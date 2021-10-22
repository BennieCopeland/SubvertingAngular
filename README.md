# SubvertingAngular
Sample project for my [blog series](https://www.benniecopeland.com/posts/subverting-angular-using-elmish) on subverting Angular using F#, Fable, and Elmish.

This project was generated with [Angular CLI](https://github.com/angular/angular-cli) version 12.2.11.

### AngularOnly Branch
This branch adds the initial Angular application that will eventually host Fable/Elmish.

Run `npx ng serve` for a dev server and navigate to `http://localhost:4200/`.

### AddingElmish Branch

This branch adds the minimum amount of code to host Fable/Elmish inside the Angular application.

Run `npx ng serve` for a dev server and navigate to `http://localhost:4200/`.

### RoutingWithRequests Branch

This branch adds multiple Elmish pages and implements the functionality to
allow Angular to request those pages based on the route.

Run `npx ng serve` for a dev server and navigate to `http://localhost:4200/`.

### RoutingWithFeliz Branch

This branch rolls back the changes of the RoutingWithRequests branch to
replace it with Feliz.Router.

Run `npx ng serve` for a dev server and navigate to `http://localhost:4200/`.