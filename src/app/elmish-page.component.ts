import {
    AfterViewInit,
    Component,
    ElementRef,
    OnDestroy,
    OnInit,
    ViewChild
} from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { v4 as uuid } from 'uuid';
import { appInit, killApp } from '@elmish/App.fs';
import { first } from 'rxjs/operators'

@Component({
    selector: 'elmish-page',
    template:
        `<div class="page">
            <div #elmishApp></div>
        </div>`
})
export class ElmishPageComponent implements
    OnInit, AfterViewInit, OnDestroy {

    @ViewChild("elmishApp") elmishApp!: ElementRef;
    pageId: string = 'unknown'

    constructor(private route: ActivatedRoute) { }

    ngOnInit() {
        this.route.data
            .pipe(first())
            .subscribe(data => {
                this.pageId = data.page
            })
    }

    ngAfterViewInit() {
        // a production app should grab this from an OIDC client
        const authToken = "FAKE AUTH TOKEN";

        let domNodeId = uuid();

        this.elmishApp.nativeElement.id = domNodeId;
        appInit(domNodeId, authToken, this.pageId);
    }

    ngOnDestroy() {
        // unmounts the react component to prevent leaking memory
        killApp(this.elmishApp.nativeElement);
    }
}
