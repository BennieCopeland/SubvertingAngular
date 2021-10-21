declare module "*App.fs" {
  function appInit(htmlId: string, authToken: string, page: string): void;

  function killApp (domNode: Element): void;
}
