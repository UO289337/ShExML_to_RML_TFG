//! Función principal

use shexml_to_rml_tfg::controller;

// use std::time::Instant;

fn main() {
    // let start = Instant::now();
    controller::compiler_controller::run();
    // let duration = start.elapsed();
    // println!("Tiempo de ejecución: {:?}", duration);
}
