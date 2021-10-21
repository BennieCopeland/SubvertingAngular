import {
  AfterViewInit,
  Component,
  ElementRef,
  OnDestroy,
  ViewChild
} from '@angular/core';
import { v4 as uuid } from 'uuid';
import { appInit, killApp } from '@elmish/App.fs';
import { Router } from '@angular/router';

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

  constructor() { }

  ngAfterViewInit() {
    // a production app should grab this from an OIDC client
    const authToken = "FAKE AUTH TOKEN";

    let domNodeId = uuid();

    this.elmishApp.nativeElement.id = domNodeId;
    appInit(domNodeId, authToken);
  }

  ngOnDestroy() {
    // unmounts the react component to prevent leaking memory
    killApp(this.elmishApp.nativeElement);
  }
}
