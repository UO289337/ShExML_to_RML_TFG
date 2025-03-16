use tinyfiledialogs;
use std::fs;
use std::io::Error;
use std::path::Path;

pub fn input_shexml_file() -> Result<String, Error> {
    loop {
        if let Some(file) = tinyfiledialogs::open_file_dialog("Select a ShExML file", "document.shexml", None) {
            let path = Path::new(&file);

            if path.extension().and_then(|ext| ext.to_str()) == Some("shexml") {
                return fs::read_to_string(file);
            } else {
                eprintln!("Error: The selected file does not have the '.shexml' extension, try again.");
            }
        } else {
            eprintln!("Select a file");
        }
    }
}