# Dog API Frontend

<details>
<summary>

## Prompt

</summary>

See https://dog.ceo/dog-api/documentation/ for general Api documentation. Write a small frontend application that

- Can be in one of two basic user interface states.
  - Dog Breed List
    - Loads the list of dog breeds/sub-breeds if not already loaded dog.ceo/api/breeds/list/all
    - Displays the list of breeds with sub-breed names displayed. The display styling need not be fancy but the list should be sorted alphabetically.
    - The user can transition to Dog Breed Details by clicking on a specific breed in the list.
  - Dog Breed Details
    - The user can return to the Dog Breed List state.
    - Loads image list from dog.ceo/api/breed/{breed}/images. A specific breed should be loaded at most once per session.
    - Displays the total number of images for the breed.
    - Displays 20 images at a time
    - Allows the user to page forward and backward with Previous and Back buttons. The buttons should only be enabled when appropriate.
- General Notes
  - Make a call to the underlying Api for a specific Url only once per application session / instance. The same data should not be fetched twice for the same run of the application so be certain to model that.
  - Always indicate to the user when data is loading and disallow interactions while loading.
  - Do not worry about fancy styling.
  - In a real application these states would be represented as routes but that complexity has been excluded here to reduce the burden of plumbing code.

</details>

## Output
Hosted on [Github Pages](https://cakekindel.github.io/panoramic-exercise-02/)

Built in / with:
 - [PureScript Halogen](https://github.com/purescript-halogen/purescript-halogen)
 - [tailwindcss](https://tailwindcss.com)
 - [bun](https://bun.sh)
 - typescript (for .js file typechecking)

Note that the [`Breed`](./src/Dog.Breed.purs) tree structure and UI are agnostic to the max
depth in the breed hierarchy, and that if the Dog CEO API were to return higher levels of nesting
than the 1 seen in the current API, this repo could be minimally changed to update.
