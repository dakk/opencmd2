#include <iostream>
#include <SDL.h>

#include <Game.h>


int main(int argc, char* argv[]) {
    try {
        Game game (argc, argv);

        return game.run();
    } catch (std::invalid_argument& ex) {
        return -2;
    } catch (std::runtime_error& ex) {
        cout << "exception" << ex.what();

        if (SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "Error", ex.what(), NULL) < 0)
            SDL_Log("Failed to show message box\n");

        SDL_Quit();

        return -1;
    }
}