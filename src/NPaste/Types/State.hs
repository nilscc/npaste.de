module NPaste.Types.State where

data NPasteState = NPasteState
  { responseFormat :: ResponseFormat
  }

data ResponseFormat
  = HtmlResponse
  | PartialHtmlResponse
  | JsonResponse
