import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ElmishPageComponent } from './elmish-page.component';
import { HomeComponent } from './home.component';

const routes: Routes = [
  {
    path: 'home',
    component: HomeComponent
  },
  {
    path: 'todos',
    component: ElmishPageComponent,
    data: { page: 'Todos'}
  },
  {
    path: 'page-a',
    component: ElmishPageComponent,
    data: { page: 'PageA'}
  },
  {
    path: 'page-b',
    component: ElmishPageComponent,
    data: { page: 'PageB'}
  },
  {
    path: 'bad-page',
    component: ElmishPageComponent,
    data: { page: 'BadPage'}
  },
  {
    path: '',
    redirectTo: '/home',
    pathMatch: 'full'
  }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
