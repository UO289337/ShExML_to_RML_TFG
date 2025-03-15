use tinyfiledialogs;

pub fn input_shexml_file(filename: &str) {
    let mut shexml_file: String = "".to_string();
    let error_msg;

    match tinyfiledialogs::open_file_dialog("Selecciona un archivo", filename, None) {
        Some(file) => shexml_file = file,
        None => error_msg = "El fichero debe encontrarse en la ruta especificada",
    }

    println!("Open file {:?}", shexml_file);
}