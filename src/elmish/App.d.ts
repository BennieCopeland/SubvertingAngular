declare module "*App.fs" {
  function appInit(
      htmlId: string,
      authToken: string,
      router: (commands: string[], skipLocationChange: boolean) => void
  ): void;

  function killApp (domNode: Element): void;
}
