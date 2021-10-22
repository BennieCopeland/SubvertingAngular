import {
    AfterViewInit,
    Component,
    ElementRef,
    NgZone,
    OnDestroy,
    ViewChild
} from '@angular/core';
import { v4 as uuid } from 'uuid';
import { appInit, killApp } from '@elmish/App.fs';
import {Event, NavigationEnd, Router } from '@angular/router';
import { Subscription } from 'rxjs';

@Component({
    selector: 'elmish-page',
    template:
        `<div class="page">
            <div #elmishApp></div>
        </div>`
})
export class ElmishPageComponent implements
    AfterViewInit, OnDestroy {

    @ViewChild("elmishApp") elmishApp!: ElementRef;
    routeSubscription!: Subscription

    constructor(
        private router: Router,
        private zone: NgZone
    ) { }

    ngOnInit() {
        this.routeSubscription =
            this.router.events.subscribe((event: Event) => {
                // On NavigationEnd, fires a custom event required by Feliz Router to trigger route detection
                if (event instanceof NavigationEnd) {
                    let ev = document.createEvent("CustomEvent")

                    ev.initEvent ("CUSTOM_NAVIGATION_EVENT_FINISHED", true, true);
                    window.dispatchEvent(ev);
                }
            })
    }

    ngAfterViewInit() {
        // a production app should grab this from an OIDC client
        const authToken = "FAKE AUTH TOKEN";

        let domNodeId = uuid();

        this.elmishApp.nativeElement.id = domNodeId;

        const navigate = (value: string[], skipLocationChange: boolean) => {
            this.zone.run(() => this.router.navigate(value, {skipLocationChange : skipLocationChange}));
        }

        appInit(domNodeId, authToken, navigate);
    }

    ngOnDestroy() {
        // unmounts the react component to prevent leaking memory
        killApp(this.elmishApp.nativeElement);
        this.routeSubscription.unsubscribe();
    }
}
