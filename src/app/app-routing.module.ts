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
    children: [
      {
        path: '**',
        component: ElmishPageComponent
      }
    ]
  },
  {
    path: 'page-a',
    component: ElmishPageComponent
  },
  {
    path: 'page-b',
    component: ElmishPageComponent
  },
  {
    path: 'bad-page',
    component: ElmishPageComponent
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
