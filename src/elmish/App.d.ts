declare module "*App.fs" {
  function appInit(htmlId: string, authToken: string): void;

  function killApp (domNode: Element): void;
}
